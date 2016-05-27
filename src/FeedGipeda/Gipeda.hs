{-# LANGUAGE OverloadedStrings #-}

{-| This module is just for parsing and generating gipeda.yaml files.
    It should probably replaced by a reference to gipeda, but it works for now.
    The raison d'être is @settingsForRepo@.
-}

module FeedGipeda.Gipeda
  ( GipedaSettings (..)
  , BenchmarkSettings (..)
  , settingsForRepo
  , determineBenchmarkScript
  ) where


import           Control.Applicative
import           Data.Aeson          (withArray, withObject)
import           Data.Functor
import           Data.Maybe          (catMaybes, fromMaybe)
import           Data.String         (fromString)
import           Data.Yaml           (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Yaml           as Yaml
import           FeedGipeda.GitShell (SHA)
import qualified FeedGipeda.GitShell as GitShell
import           FeedGipeda.Repo     (Repo)
import qualified FeedGipeda.Repo     as Repo
import           System.Directory    (doesFileExist)
import           Text.Printf         (printf)


{-| Models the possible settings in a @gipeda.yaml@ file, as expected to be
    present by @gipeda@.
-}
data GipedaSettings = GipedaSettings
  { title               :: String
  -- ^ Sets the repo-specific <title> text.
  , revisionInfo        :: String
  {-^ Arbitrary HTML code that will displayed on a single commit's page.
      @{{rev}}@ is replaced by the full SHA hash of the commit.
  -}
  , diffLink            :: String
  {-^ A link that displays the difference between two commits of the repository.
      The strings @{{rev}}@ and @{{base}}@ are replaced by the full SHA hashes of
      the two commits to compare.
  -}
  , limitRecent         :: Int
  -- ^ Number of commits shown on the start page.
  , start               :: SHA
  {-^ Commit hash of the first commit to take into account. Useful to limit
      the scope of gipeda in projects with a large history.
  -}
  , interestingTags     :: String -- wildcard based, i.e. "*"
  {-^ A glob (as understood by git tag -l) specifying which tags should be
      shown on the main page. By default, no tags are shown; use * to show all.
  -}
  , interestingBranches :: String
  -- ^ A glob such as @interestingTags@, but for selecting branches.
  , benchmarkScript     :: String
  {-^ A shell command to execute in the directory for benchmarking.
      Should produce CSV data in the format expected by gipeda.
  -}
  , benchmarks          :: [BenchmarkSettings]
  -- ^ Matchers for benchmark groups within the CSV files.
  } deriving (Show)


{-| Matches benchmark groups from the names in the produced benchmark CSV files.
-}
data BenchmarkSettings = BenchmarkSettings
  { match           :: String
  {-^ A file glob that determines to what benchmarks these settings apply.
      @match: "*"@ will apply these settings to all benchmarks,
      @match: "regression/*"*@ only to those whose title starts with
      @regression/@.
  -}
  , smallerIsBetter :: Maybe Bool
  {-^ By default (or if this is @False@), gipeda assumes that greater number
      indicate improvement, and smaller number indicate regressions
      (e.g. requests per second). This inverts this logic (e.g. runtime).
  -}
  , unit            :: Maybe String
  -- ^ Arbitrary unit, to be printed in the tables and on the graph axes. Defaults to @""@.
  , type_           :: Maybe String
  {-^ @"float", @"integral"@ (default) or @"small integral". For the first two,
      differences are expressed in percentages (+10%), while for the latter,
      differences are expressed in differences (+2). This is more suitable for
      numbers like test suite failures, which should usually be zero or very
      small.
  -}
  , group           :: Maybe String
  {-^ The benchmarks are displayed in groups; all benchmarks that have the same
      string here are grouped under this title. Defaults to @""@.
  -}
  , threshold       :: Maybe Int
  {-^ Any change by a percentage greater than this number is flagged as a
      regression resp. improvement; anything below this number is considered to
      be a boring result, and not highlighted separately.
  -}
  , important       :: Maybe Bool
  {-^ @True@ by default. If @False@, a regression in this benchmark will not be
      included in the summary for the whole commit. Use this if you have very
      volatile tests that would produce too much noise on the main page.
  -}
  } deriving (Show)


benchmark :: String -> BenchmarkSettings
benchmark match = BenchmarkSettings
  { match = match
  , smallerIsBetter = Nothing
  , unit = Nothing
  , type_ = Nothing
  , group = Nothing
  , threshold = Nothing
  , important = Nothing
  }


instance ToJSON BenchmarkSettings where
  toJSON s = (Yaml.object . catMaybes)
    [ "match" .=? Just (match s)
    , "smallerIsBetter" .=? smallerIsBetter s
    , "unit" .=? unit s
    , "type" .=? type_ s
    , "group" .=? group s
    , "threshold" .=? threshold s
    , "important" .=? important s
    ] where key .=? value = fmap (key .=) value


instance FromJSON BenchmarkSettings where
  parseJSON (Yaml.Object o) = BenchmarkSettings
    <$> o .: "match"
    <*> o .? "smallerIsBetter"
    <*> o .? "unit"
    <*> o .? "type"
    <*> o .? "group"
    <*> o .? "threshold"
    <*> o .? "important"
    where o .? key = optional (o .: key)


instance ToJSON GipedaSettings where
  toJSON s = Yaml.object
    [ "title" .= title s
    , "revisionInfo" .= revisionInfo s
    , "diffLink" .= diffLink s
    , "limitRecent" .= limitRecent s
    , "start" .= start s
    , "interestingTags" .= interestingTags s
    , "interestingBranches" .= interestingBranches s
    , "benchmarkScript" .= benchmarkScript s
    , "benchmarks" .= benchmarks s
    ]


{-| Determines the benchmark script for the given repo by looking into the
    @gipeda.yaml@ file in the project-specific directory under the working
    directory. The settings file was previously generated by @settingsForRepo@,
    which also tries to merge project-specific settings to the default.
-}
determineBenchmarkScript :: Repo -> IO String
determineBenchmarkScript repo = do
  settingsFile <- Repo.settingsFile repo
  exists <- doesFileExist settingsFile
  maybeSettings <- if exists then Yaml.decodeFile settingsFile else return Nothing
  return $ fromMaybe "cloben" $ do
    (Yaml.Object settings) <- maybeSettings
    Yaml.parseMaybe (\_ -> settings .: "benchmarkScript") ()


{-| Generates a @gipeda.yaml@ file for the given repository. It thereby takes
    project-specific settings from a top-level @gipeda.yaml@ file at the
    repository's @HEAD@ (if present) and fills in missing settings with
    defaults.
-}
settingsForRepo :: Repo -> IO GipedaSettings
settingsForRepo repo = do
  clone <- Repo.cloneDir repo
  firstCommit <- GitShell.firstCommit clone
  gipedaYaml <- GitShell.showHead clone "gipeda.yaml"
  let
    yaml :: Yaml.Value
    yaml =
      fromMaybe (Yaml.object []) (gipedaYaml >>= Yaml.decode . fromString)

    revisionInfo :: String
    revisionInfo =
      printf "<a href=\"%s/commit/{{rev}}>View Diff</a>" (Repo.uri repo)

    settings :: Yaml.Value -> Yaml.Parser GipedaSettings
    settings (Yaml.Object obj) =
      GipedaSettings
        <$> "title" ?? Repo.shortName repo
        <*> "revisionInfo" ?? printf "<a href=\"%s/commit/{{rev}}>View Diff</a>" (Repo.uri repo)
        <*> "diffLink" ?? "{{rev}}{{base}}" -- TODO
        <*> "limitRecent" ?? 20
        <*> "start" ?? fromMaybe "HEAD" firstCommit -- "HEAD" doesn't really work, but better than crashing?! We shouldn't execute gipeda on an empty repository after all
        <*> "interestingTags" ?? "*"
        <*> "interestingBranches" ?? "*"
        <*> "benchmarkScript" ?? "cloben"
        <*> "benchmarks" ?? []
        where key ?? def = obj .: key <|> pure def
    settings _ = settings (Yaml.object []) -- This should fill in some defaults nonetheless

  Yaml.parseMonad settings yaml -- This should never fail. Never!

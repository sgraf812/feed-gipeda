{-# LANGUAGE OverloadedStrings #-}

{-| This module is just for parsing and generating gipeda.yaml files.
    It should probably replaced by a reference to gipeda, but it works for now.
    The real raison d'être is @settingsForRepo@, which is rather an euphemism,
    since in reality it can only generate settings for @cloben@ output.
-}

module Gipeda
  ( GipedaSettings (..)
  , BenchmarkSettings (..)
  , settingsForRepo
  , determineBenchmarkScript
  ) where


import           Control.Applicative (optional, (<|>))
import           Data.Aeson          (withArray, withObject)
import           Data.Maybe          (catMaybes, fromMaybe)
import           Data.String         (fromString)
import           Data.Yaml           (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Yaml           as Yaml
import           GitShell            (SHA)
import qualified GitShell
import           Repo                (Repo)
import qualified Repo
import           Text.Printf         (printf)


data GipedaSettings = GipedaSettings
  { title               :: String
  , revisionInfo        :: String
  , diffLink            :: String
  , limitRecent         :: Int
  , start               :: SHA
  , interestingTags     :: String -- wildcard based, i.e. "*"
  , interestingBranches :: String
  , benchmarkScript     :: String
  , benchmarks          :: [BenchmarkSettings] -- This should be determined by the benchmark script used
  } deriving (Show)


data BenchmarkSettings = BenchmarkSettings
  { match           :: String
  , smallerIsBetter :: Maybe Bool -- defaults to true
  , unit            :: Maybe String          -- defaults to ""
  , type_           :: Maybe String         -- defaults to "integral"
  , group           :: Maybe String         -- defaults to ""
  , threshold       :: Maybe Int        -- defaults to 3
  , important       :: Maybe Bool       -- defaults to true
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


determineBenchmarkScript :: Repo -> IO String
determineBenchmarkScript repo = do
  settingsFile <- Repo.settingsFile repo
  maybeSettings <- Yaml.decodeFile settingsFile
  return $ fromMaybe "cloben" $ do
    (Yaml.Object settings) <- maybeSettings
    Yaml.parseMaybe (\_ -> settings .: "benchmarkScript") ()


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

    warnings :: BenchmarkSettings
    warnings =
      (benchmark "build/warnings")
        { smallerIsBetter = Just False
        , group = Just "Build"
        , type_ = Just "small integral" , important = Just False
        }

    settings :: Yaml.Value -> Yaml.Parser GipedaSettings
    settings (Yaml.Object obj) =
      GipedaSettings
        <$> "title" ?? Repo.shortName repo
        <*> "revisionInfo" ?? printf "<a href=\"%s/commit/{{rev}}>View Diff</a>" (Repo.uri repo)
        <*> "diffLink" ?? "{{rev}}{{base}}" -- TODO
        <*> "limitRecent" ?? 20
        <*> "start" ?? firstCommit
        <*> "interestingTags" ?? "*"
        <*> "interestingBranches" ?? "*"
        <*> "benchmarkScript" ?? "cloben"
        <*> ((++ [warnings]) <$> "benchmarks" ?? [])
        where key ?? def = obj .: key <|> pure def
    settings _ = settings (Yaml.object []) -- This should fill in some defaults nonetheless

  Yaml.parseMonad settings yaml -- This should never fail. Never!

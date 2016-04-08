{-# LANGUAGE OverloadedStrings #-}

{-| This module is just for parsing and generating gipeda settings.yaml files.
    It should probably replaced by a reference to gipeda, but it works for now.
    The real raison d'être is @settingsForRepo@, which is rather an euphemism,
    since in reality it can only generate settings for @cloben@ output.
-}

module Gipeda
  ( GipedaSettings (..)
  , BenchmarkSettings (..)
  , settingsForRepo
  ) where


import           Data.Maybe  (catMaybes)
import           Data.Yaml   (FromJSON (..), ToJSON (..), (.=))
import qualified Data.Yaml   as Yaml
import           GitShell    (SHA)
import qualified GitShell
import           Repo        (Repo)
import qualified Repo
import           Text.Printf (printf)


data GipedaSettings = GipedaSettings
  { title           :: String
  , diffLink        :: String
  , revisionInfo    :: String
  , limitRecent     :: Int
  , start           :: SHA
  , interestingTags :: String -- wildcard based, i.e. "*"
  , benchmarks      :: [BenchmarkSettings] -- This should be determined by the benchmark script used
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


instance ToJSON GipedaSettings where
  toJSON s = Yaml.object
    [ "title" .= title s
    , "diffLink" .= diffLink s
    , "revisionInfo" .= revisionInfo s
    , "limitRecent" .= limitRecent s
    , "start" .= start s
    , "interestingTags" .= interestingTags s
    , "benchmarks" .= benchmarks s
    ]


settingsForRepo :: Repo -> IO GipedaSettings
settingsForRepo repo = do
  clone <- Repo.cloneDir repo
  firstCommit <- GitShell.firstCommit clone
  let
    revisionInfo =
      printf "<a href=\"%s/commit/{{rev}}>View Diff</a>" (Repo.uri repo)
  return GipedaSettings
    { title = Repo.shortName repo
    , diffLink = "{{rev}}{{base}}" -- TODO
    , revisionInfo = revisionInfo
    , limitRecent = 20
    , start = firstCommit
    , interestingTags = "*"
    , benchmarks =
        [ (benchmark "benchmark/*")
            { group = Just "Benchmark"
            , unit = Just "ns"
            , type_ = Just "float"
            }
        , (benchmark "build/warnings")
            { smallerIsBetter = Just False
            , group = Just "Build"
            , type_ = Just "small integral"
            , important = Just False
            }
        ]
    }

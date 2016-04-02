{-# LANGUAGE OverloadedStrings #-}

module Repo
  ( Repo (..)
  , uri
  , Repos ()
  , noRepos
  , toList
  , difference
  , uniqueName
  , shortName
  , projectDir
  , cloneDir
  , resultsDir
  ) where

import qualified Data.Hash        as Hash
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Word        (Word64)
import           Data.Yaml        (FromJSON (..), Value (Object), (.:))
import           Network.URI
import           System.Directory (getCurrentDirectory)
import           System.FilePath  (takeFileName, (</>))

newtype Repo = Repo
  { unRepo :: URI
  } deriving (Eq, Ord, Show)


uri :: Repo -> String
uri (Repo u) =
  uriToString id u ""


newtype Repos = Repos
  { unRepos :: Set Repo
  } deriving (Show)


noRepos :: Repos
noRepos =
  Repos Set.empty


toList :: Repos -> [Repo]
toList =
  Set.toList . unRepos


difference :: Repos -> Repos -> Repos
difference (Repos a) (Repos b) =
  Repos (Set.difference a b)


instance FromJSON Repo where
  parseJSON v = do
    s <- parseJSON v
    uri <- maybe (fail "Could not parse URI") return (parseURI s)
    return (Repo uri)


hash :: Repo -> Word64
hash =
  Hash.asWord64 . Hash.hash . show . uri


uniqueName :: Repo -> String
uniqueName repo =
  shortName repo ++ "-" ++ (show . hash) repo


shortName :: Repo -> String
shortName repo =
  takeFileName (uriPath (unRepo repo))


instance FromJSON Repos where
  parseJSON (Object v) =
    fmap (Repos . Set.fromList) (v .: "repositories")
  parseJSON _ =
    fail "Object"


projectDir :: Repo -> IO FilePath
projectDir repo = do
  cwd <- getCurrentDirectory
  return (cwd </> Repo.uniqueName repo)


cloneDir :: Repo -> IO FilePath
cloneDir repo =
  fmap (</> "repository") (projectDir repo)


resultsDir :: Repo -> IO FilePath
resultsDir repo =
  fmap (</> "site" </> "out" </> "results") (projectDir repo)

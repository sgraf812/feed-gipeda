{-# LANGUAGE OverloadedStrings #-}

{-| Provides domain functions about remote git repositories.
    It also handles YAML parsing and @projectDir@ related stuff.
    All-in-all, this is a little messy and should undergo a refactoring
    if there should come any major changes to the code base.
-}

module FeedGipeda.Repo
  ( Repo (..)
  , unsafeFromString
  , uri
  , uniqueName
  , shortName
  , projectDir
  , cloneDir
  , resultsDir
  , settingsFile
  , backlogFile
  ) where


import           Data.Binary      (Binary (..))
import qualified Data.Hash        as Hash
import           Data.Maybe       (fromJust)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Word        (Word64)
import           Network.URI      (URI, parseURI, uriPath, uriToString)
import           System.Directory (getCurrentDirectory)
import           System.FilePath  (takeFileName, (</>))


newtype Repo = Repo
  { unRepo :: URI
  } deriving (Eq, Ord, Show)


instance Binary Repo where
  get = unsafeFromString <$> get
  put = put . uri


unsafeFromString :: String -> Repo
unsafeFromString uri =
  Repo (fromJust (parseURI uri))


uri :: Repo -> String
uri (Repo u) =
  uriToString id u ""


hash :: Repo -> Word64
hash =
  Hash.asWord64 . Hash.hash . show . uri


uniqueName :: Repo -> String
uniqueName repo =
  shortName repo ++ "-" ++ (show . hash) repo


shortName :: Repo -> String
shortName repo =
  takeFileName (uriPath (unRepo repo))


projectDir :: Repo -> IO FilePath
projectDir repo = do
  cwd <- getCurrentDirectory
  return (cwd </> uniqueName repo)


cloneDir :: Repo -> IO FilePath
cloneDir repo =
  fmap (</> "repository") (projectDir repo)


resultsDir :: Repo -> IO FilePath
resultsDir repo =
  fmap (</> "site" </> "out" </> "results") (projectDir repo)


settingsFile :: Repo -> IO FilePath
settingsFile repo =
  fmap (</> "gipeda.yaml") (projectDir repo)


backlogFile :: Repo -> IO FilePath
backlogFile repo =
  fmap (</> "backlog.txt") (projectDir repo)

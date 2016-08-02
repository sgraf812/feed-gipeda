{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

{-| Provides domain functions around remote git repositories.
    It also exposes @projectDir@ related functions, e.g. mapping conceptual file
    paths to concrete file paths.
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
import           Data.Functor
import qualified Data.Hash        as Hash
import           Data.Maybe       (fromJust)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Typeable    (Typeable)
import           Data.Word        (Word64)
import           Network.URI      (URI, parseURI, uriPath, uriToString)
import           System.Directory (getCurrentDirectory)
import           System.FilePath  (takeFileName, (</>))


newtype Repo = Repo
  { unRepo :: URI
  } deriving (Eq, Ord, Show, Typeable)


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


{-| Used in @projectDir@ to disambiguate file paths. -}
uniqueName :: Repo -> String
uniqueName repo =
  shortName repo ++ "-" ++ (show . hash) repo


shortName :: Repo -> String
shortName repo =
  takeFileName (uriPath (unRepo repo))


{-| Assigns each repository a unique local working directory. -}
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
  fmap (</> "site" </> "out" </> "backlog.txt") (projectDir repo)

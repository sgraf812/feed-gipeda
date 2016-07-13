{-| Functions for shelling out to @git@ to work with git repositories.
-}

{-# LANGUAGE OverloadedStrings #-}

module FeedGipeda.GitShell
  ( isRepositoryRoot
  , fetch
  , allCommits
  , firstCommit
  , remoteRepo
  , showHead
  , sync
  , SHA
  ) where


import           Data.Char        (isSpace)
import           Data.Functor
import           Data.Maybe       (listToMaybe)
import           Data.Monoid      ((<>))
import           Data.Set         (Set)
import qualified Data.Set         as Set
import qualified Data.Text        as Text
import           FeedGipeda.Repo  (Repo)
import qualified FeedGipeda.Repo  as Repo
import           Shelly           (Sh, shelly)
import qualified Shelly
import           System.Directory (createDirectoryIfMissing)


type SHA
  = String


isRepositoryRoot :: FilePath -> IO Bool
isRepositoryRoot path = shelly $ do
  stdout  <- Shelly.run "git" ["-C", Text.pack path, "rev-parse", "--git-dir"]
  -- testing for ".git" and "." (bare repo) should be good enough.
  (return . maybe False (`elem` [".git", "."]) . listToMaybe . lines) (Text.unpack stdout)


mirror :: Repo -> FilePath -> IO ()
mirror repo path = shelly $
  Shelly.run_ "git" ["clone", "--mirror", "--quiet", Text.pack (Repo.uri repo), Text.pack path]


remoteRepo :: FilePath -> IO Repo
remoteRepo path = shelly $ do
  stdout  <- Shelly.run "git" ["-C", Text.pack path, "ls-remote", "--get-url", "origin"]
  return (Repo.unsafeFromString (init (Text.unpack stdout))) -- strip the \n with init


fetch :: FilePath -> IO ()
fetch path = shelly $
  Shelly.run_ "git" ["-C", Text.pack path, "fetch", "--quiet"]


{-| @sync repo@ tries to fetch updates from the remote @repo@ or creates a
    mirror of @repo@ if there isn't already a local clone present.
-}
sync :: Repo -> IO ()
sync repo = do
  path <- Repo.cloneDir repo
  hasClone <- isRepositoryRoot path
  if hasClone
    then fetch path
    else do
      createDirectoryIfMissing True path
      mirror repo path


allCommits :: FilePath -> IO (Set SHA)
allCommits path =
  Set.fromList <$> gitLogImpl path []


firstCommit :: FilePath -> IO (Maybe SHA)
firstCommit path =
  listToMaybe <$> gitLogImpl path ["--reverse"]


showHead :: FilePath -> FilePath -> IO (Maybe String)
showHead path file = shelly $ do
  stdout <- Shelly.errExit False $
    Shelly.run "git" ["-C", Text.pack path, "show", "HEAD:" <> Text.pack file]
  exitCode <- Shelly.lastExitCode
  if exitCode == 0
    then return (Just (Text.unpack stdout))
    else return Nothing


gitLogImpl :: FilePath -> [String] -> IO [SHA]
gitLogImpl path additionalArgs = shelly $ do
  let
    allArgs = ["-C", path, "log", "--format=format:%H"] ++ additionalArgs
  stdout <- Shelly.run "git" (map Text.pack allArgs)
  return . filter (not . null) . lines . Text.unpack $ stdout

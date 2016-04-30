module GitShell
  ( isRepositoryRoot
  , cloneBare
  , fetch
  , allCommits
  , firstCommit
  , remoteRepo
  , sync
  , SHA
  ) where

import           Data.Char        (isSpace)
import           Data.Maybe       (listToMaybe)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Repo             (Repo)
import qualified Repo
import           System.Directory (createDirectoryIfMissing)
import           System.Exit      (ExitCode (ExitSuccess))
import           System.Process   (callProcess, readProcessWithExitCode)


type SHA
  = String


isRepositoryRoot :: FilePath -> IO Bool
isRepositoryRoot path = do
  (_, stdout, _)  <- readProcessWithExitCode
    "git" ["-C", path, "rev-parse", "--git-dir"] ""
  -- testing for ".git" and "." (bare repo) should be good enough.
  (return . maybe False (`elem` [".git", "."]) . listToMaybe . lines) stdout


cloneBare :: Repo -> FilePath -> IO ()
cloneBare repo path =
  callProcess "git" ["clone", "--bare", "--single-branch", "--quiet", Repo.uri repo, path]


remoteRepo :: FilePath -> IO Repo
remoteRepo path = do
  (_, stdout, _)  <- readProcessWithExitCode
    "git" ["-C", path, "ls-remote", "--get-url", "origin"] ""
  return (Repo.unsafeFromString stdout)


fetch :: FilePath -> IO ()
fetch path =
  callProcess "git" ["-C", path, "fetch", "--quiet"]


sync :: Repo -> IO ()
sync repo = do
  path <- Repo.cloneDir repo
  hasClone <- GitShell.isRepositoryRoot path
  if hasClone
    then GitShell.fetch path
    else do
      createDirectoryIfMissing True path
      GitShell.cloneBare repo path


allCommits :: FilePath -> IO (Set SHA)
allCommits path =
  Set.fromList <$> gitLogImpl path []


firstCommit :: FilePath -> IO SHA
firstCommit path =
  head <$> gitLogImpl path ["--reverse"]


gitLogImpl :: FilePath -> [String] -> IO [SHA]
gitLogImpl path additionalArgs = do
  let
    allArgs = ["-C", path, "log", "--format=format:%H"] ++ additionalArgs
  (_, stdout, _) <- readProcessWithExitCode "git" allArgs ""
  return (filter (not . null) (lines stdout))

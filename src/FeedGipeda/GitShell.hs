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
import           Data.Maybe       (listToMaybe)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           FeedGipeda.Repo  (Repo)
import qualified FeedGipeda.Repo  as Repo
import           System.Directory (createDirectoryIfMissing)
import           System.Exit      (ExitCode (..))
import           System.Process   (callProcess, readProcessWithExitCode)


type SHA
  = String


isRepositoryRoot :: FilePath -> IO Bool
isRepositoryRoot path = do
  (_, stdout, _)  <- readProcessWithExitCode
    "git" ["-C", path, "rev-parse", "--git-dir"] ""
  -- testing for ".git" and "." (bare repo) should be good enough.
  (return . maybe False (`elem` [".git", "."]) . listToMaybe . lines) stdout


mirror :: Repo -> FilePath -> IO ()
mirror repo path =
  callProcess "git" ["clone", "--mirror", "--quiet", Repo.uri repo, path]


remoteRepo :: FilePath -> IO Repo
remoteRepo path = do
  (_, stdout, _)  <- readProcessWithExitCode
    "git" ["-C", path, "ls-remote", "--get-url", "origin"] ""
  return (Repo.unsafeFromString (init stdout)) -- strip the \n with init


fetch :: FilePath -> IO ()
fetch path =
  callProcess "git" ["-C", path, "fetch", "--quiet"]


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
showHead path file = do
  let
    allArgs = ["-C", path, "show", "HEAD:" ++ file]
  (exitCode, stdout, stderr) <- readProcessWithExitCode "git" allArgs ""
  case exitCode of
    ExitSuccess -> return (Just stdout)
    ExitFailure _ -> return Nothing


gitLogImpl :: FilePath -> [String] -> IO [SHA]
gitLogImpl path additionalArgs = do
  let
    allArgs = ["-C", path, "log", "--format=format:%H"] ++ additionalArgs
  (_, stdout, _) <- readProcessWithExitCode "git" allArgs ""
  return (filter (not . null) (lines stdout))

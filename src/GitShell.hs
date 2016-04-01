module GitShell
  ( cloneBare
  , fetch
  , allCommits
  , SHA
  ) where

import           Data.Char      (isSpace)
import           Data.Set       (Set)
import qualified Data.Set       as Set
import           Repo           (Repo)
import qualified Repo
import           System.Process (callProcess, cwd, proc, readCreateProcessWithExitCode)


type SHA
  = String


cloneBare :: Repo -> FilePath -> IO ()
cloneBare repo path =
  callProcess "git" ["clone", "--bare", "--single-branch", "--quiet", Repo.uri repo, path]


fetch :: FilePath -> IO ()
fetch path = do
  readCreateProcessWithExitCode
    (proc "git" ["fetch", "--quiet"]) { cwd = Just path }
    ""
  return ()


allCommits :: FilePath -> IO (Set SHA)
allCommits path =
  fmap Set.fromList (gitLogImpl path [])


gitLogImpl :: FilePath -> [String] -> IO [SHA]
gitLogImpl path additionalArgs = do
  let
    allArgs = ["log", "--format=format:%H"] ++ additionalArgs
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode
    (proc "git" allArgs) { cwd = Just path }
    ""
  return (filter (not . null) (lines stdout))

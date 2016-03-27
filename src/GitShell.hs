module GitShell
  ( cloneBare
  , revParseHead
  , fetch
  , commitsInRange
  , allCommits
  , SHA
  ) where

import           Repo           (Repo)
import qualified Repo
import           System.Process (callProcess, cwd, proc, readCreateProcess)


type SHA
  = String


cloneBare :: Repo -> FilePath -> IO ()
cloneBare repo path =
  callProcess "git" ["clone", "--bare", Repo.uri repo, path]


revParseHead :: FilePath -> IO SHA
revParseHead path =
  readCreateProcess
    (proc "git" ["rev-parse", "HEAD"]) { cwd = Just path }
    ""


fetch :: FilePath -> IO ()
fetch path = do
  readCreateProcess
    (proc "git" ["fetch"]) { cwd = Just path }
    ""
  return ()


commitsInRange :: FilePath -> String -> String -> IO [SHA]
commitsInRange path lower upper =
  gitLogImpl path [lower ++ ".." ++ upper]


allCommits :: FilePath -> IO [SHA]
allCommits path =
  gitLogImpl path []


gitLogImpl :: FilePath -> [String] -> IO [SHA]
gitLogImpl path additionalArgs = do
  let
    allArgs = ["log", "--format=format:%H"] ++ additionalArgs
  output <- readCreateProcess
    (proc "git" allArgs) { cwd = Just path }
    ""
  return (lines output)

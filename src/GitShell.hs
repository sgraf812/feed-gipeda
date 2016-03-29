module GitShell
  ( cloneBare
  , revParseHead
  , fetch
  , commitsInRange
  , allCommits
  , SHA
  ) where

import           Data.Char      (isSpace)
import           Data.Set       (Set)
import qualified Data.Set       as Set
import           Repo           (Repo)
import qualified Repo
import           System.Process (callProcess, cwd, proc, readCreateProcess)


type SHA
  = String


cloneBare :: Repo -> FilePath -> IO ()
cloneBare repo path =
  callProcess "git" ["clone", "--bare", "--single-branch", "--quiet", Repo.uri repo, path]


revParseHead :: FilePath -> IO SHA
revParseHead path = do
  output <- readCreateProcess
    (proc "git" ["rev-parse", "HEAD"]) { cwd = Just path }
    ""
  return (filter (not . isSpace) output)


fetch :: FilePath -> IO ()
fetch path = do
  readCreateProcess
    (proc "git" ["fetch", "--quiet"]) { cwd = Just path }
    ""
  return ()


commitsInRange :: FilePath -> String -> String -> IO [SHA]
commitsInRange path lower upper =
  gitLogImpl path [lower ++ ".." ++ upper]


allCommits :: FilePath -> IO (Set SHA)
allCommits path =
  fmap Set.fromList (gitLogImpl path [])


gitLogImpl :: FilePath -> [String] -> IO [SHA]
gitLogImpl path additionalArgs = do
  let
    allArgs = ["log", "--format=format:%H"] ++ additionalArgs
  output <- readCreateProcess
    (proc "git" allArgs) { cwd = Just path }
    ""
  return (filter (not . null) (lines output))

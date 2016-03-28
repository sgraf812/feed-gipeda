{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent.MVar
import           Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forever)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Par.IO     (runParIO)
import           Data.Maybe               (fromJust)
import           Data.String              (fromString)
import qualified Data.Yaml                as Yaml
import           GitShell                 (SHA)
import qualified GitShell
import           Repo                     (Repo, Repos)
import qualified Repo
import           System.Console.ArgParser (Descr (..), ParserSpec, andBy, parsedBy,
                                           optPos, withParseResult)
import           System.Directory
import           System.FilePath          ((</>))
import           Twitch                   ((|>))
import qualified Twitch
import qualified System.FSNotify as FS
import qualified Data.Time as Time


parseRepos :: FilePath -> IO Repos
parseRepos f = do
  repos <- Yaml.decodeFile f
  -- TODO: error handling
  return (fromJust repos)


repoPath :: Repo -> IO FilePath
repoPath repo = do
  cwd <- getCurrentDirectory
  return (cwd </> Repo.uniqueName repo </> "repository")


benchmark :: String -> FilePath -> SHA -> IO ()
benchmark script path commit =

  putStrLn (path ++ ":" ++ commit)


fetchRepos :: String -> Repos -> IO ()
fetchRepos script repos =
  mapM_ fetchRepo (Repo.toList repos)
    where
      fetchRepo :: Repo -> IO ()
      fetchRepo repo = do
        newCommits <- getNewCommits repo
        path <- repoPath repo
        mapM_ (runParIO . liftIO . benchmark script path) newCommits


getNewCommits :: Repo -> IO [SHA]
getNewCommits repo = do
  path <- repoPath repo
  exists <- doesDirectoryExist path
  if exists
    then do
      oldHead <- GitShell.revParseHead path
      GitShell.fetch path
      newHead <- GitShell.revParseHead path
      GitShell.commitsInRange path oldHead newHead
    else do
      createDirectoryIfMissing True path
      GitShell.cloneBare repo path
      GitShell.allCommits path


updateRepos :: String -> MVar Repos -> FilePath -> IO ()
updateRepos script reposVar f = do
  !diff <- modifyMVar reposVar swapAndComputeDiff
  fetchRepos script diff
    where
      swapAndComputeDiff oldList = do
        newList <- parseRepos f
        return (newList, Repo.difference newList oldList)


data CmdArgs
  = CmdArgs
  { benchmarkScript :: String
  }


cmdParser :: ParserSpec CmdArgs
cmdParser = CmdArgs
  `parsedBy` optPos "cloben" "benchmark" `Descr` "benchmark script which will be"
  ++ " supplied the repository to name and specific commit to benchmark"


withWatchDep :: Twitch.Dep -> (FS.WatchManager -> IO a) -> IO a
withWatchDep dep =
  bracket (Twitch.run dep) FS.stopManager


periodicallyUpdate :: Time.NominalDiffTime -> String -> MVar Repos -> IO ()
periodicallyUpdate dt script repos = forever $ do
  begin <- Time.getCurrentTime
  readMVar repos >>= fetchRepos script
  end <- Time.getCurrentTime
  let elapsed = Time.diffUTCTime end begin
  threadDelay (ceiling ((dt - elapsed) * 1000000))


main :: IO ()
main = withParseResult cmdParser $ \(CmdArgs script) -> do
  repos <- newMVar Repo.noRepos
  withWatchDep
    ("feed-gipeda.yaml" |> updateRepos script repos)
    (\_ -> do
      updateRepos script repos "feed-gipeda.yaml"
      periodicallyUpdate 5 script repos)

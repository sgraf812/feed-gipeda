{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent.MVar
import           Control.Monad.Par.IO    (runParIO)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe              (fromJust)
import           Data.String             (fromString)
import qualified Data.Yaml               as Yaml
import           GitShell                (SHA)
import qualified GitShell
import           Repo                    (Repo, Repos)
import qualified Repo
import           System.Directory
import           System.FilePath         ((</>))
import           Twitch                  ((|>))
import qualified Twitch


parseRepos :: FilePath -> IO Repos
parseRepos f = do
  repos <- Yaml.decodeFile f
  -- TODO: error handling
  return (fromJust repos)


repoPath :: Repo -> IO FilePath
repoPath repo = do
  cwd <- getCurrentDirectory
  return (cwd </> Repo.uniqueName repo </> "repository")


cloben :: Repo -> SHA -> IO ()
cloben repo commit =
  putStrLn (Repo.uri repo ++ ":" ++ commit)


fetchRepo :: Repo -> IO ()
fetchRepo repo = do
  newCommits <- getNewCommits repo
  mapM_ (runParIO . liftIO . cloben repo) newCommits
  return ()


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


updateRepos :: MVar Repos -> FilePath -> IO ()
updateRepos reposVar f = do
  !diff <- modifyMVar reposVar swapAndComputeDiff
  mapM_ fetchRepo (Repo.toList diff)
    where
      swapAndComputeDiff oldList = do
        newList <- parseRepos f
        return (newList, Repo.difference newList oldList)


main :: IO ()
main = do
  repos <- newMVar Repo.noRepos
  updateRepos repos "feed-gipeda.yaml" -- TODO: Fishy
  Twitch.defaultMain ("feed-gipeda.yaml" |> updateRepos repos)

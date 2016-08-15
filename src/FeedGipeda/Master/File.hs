-- | Functions for file handling in the master node.

module FeedGipeda.Master.File
  ( writeBenchmarkCSV
  , isBenchmarkCSV
  , readBacklog
  , isBacklog
  , repoOfPath
  ) where


import           Data.Char           (toLower)
import           Data.Functor
import           Data.List           (isSuffixOf)
import           Data.Maybe          (listToMaybe)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           FeedGipeda.GitShell (SHA)
import qualified FeedGipeda.GitShell as GitShell
import           FeedGipeda.Prelude
import           FeedGipeda.Repo     (Repo)
import qualified FeedGipeda.Repo     as Repo
import           System.Directory    (createDirectoryIfMissing, doesFileExist,
                                      getCurrentDirectory, getDirectoryContents)
import           System.FilePath     (dropFileName, makeRelative, normalise,
                                      splitDirectories, takeBaseName,
                                      takeBaseName, takeExtension, takeFileName,
                                      (<.>), (</>))


readBacklog :: Repo -> IO [SHA]
readBacklog repo = do
  backlog <- Repo.backlogFile repo
  maybe [] lines <$> readFileMaybe backlog


matchProjectRelativeDirectory :: [String] -> FilePath -> FilePath -> Bool
matchProjectRelativeDirectory subDirs cwd path =
  let
    splitNormalisedDirs :: [String]
    splitNormalisedDirs =
      splitDirectories (makeRelative cwd (normalise (dropFileName path)))
  in
    length splitNormalisedDirs == length subDirs + 1
     && subDirs `isSuffixOf` splitNormalisedDirs


isBacklog :: FilePath -> FilePath -> Bool
isBacklog cwd path =
  and
    [ map toLower (takeBaseName path) == "backlog"
    , map toLower (takeExtension path) == ".txt"
    , matchProjectRelativeDirectory ["site", "out"] cwd path
    ]


writeBenchmarkCSV :: Repo -> SHA -> String -> IO ()
writeBenchmarkCSV repo commit result = do
  resultsDir <- Repo.resultsDir repo
  logInfo ("Writing benchmark CSV file to " ++ (resultsDir </> commit <.> "csv"))
  cwd <- getCurrentDirectory
  createDirectoryIfMissing True resultsDir
  writeFile (resultsDir </> commit <.> "csv") result


isBenchmarkCSV :: FilePath -> FilePath -> Bool
isBenchmarkCSV cwd path =
  map toLower (takeExtension path) == ".csv"
   && matchProjectRelativeDirectory ["site", "out", "results"] cwd path


repoOfPath :: FilePath -> Set Repo -> FilePath -> IO (Maybe Repo)
repoOfPath cwd activeRepos path =
  let
    uniqueName :: FilePath -> Maybe String
    uniqueName =
      listToMaybe . splitDirectories . makeRelative cwd
  in
    case uniqueName path of
      Nothing -> return Nothing
      Just name -> do
        let path = cwd </> name </> "repository"
        hasClone <- GitShell.isRepositoryRoot path
        if not hasClone
          then return Nothing
          else do
            repo <- GitShell.remoteRepo path
            if not (Set.member repo activeRepos)
              then return Nothing
              else return (Just repo)

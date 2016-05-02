module Master.File
  ( writeBenchmarkCSV
  , isBenchmarkCSV
  , writeBacklog
  , readBacklog
  , isBacklog
  , repoOfPath
  ) where


import           Data.Char        (toLower)
import           Data.List        (isSuffixOf)
import           Data.Maybe       (listToMaybe)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           GitShell         (SHA)
import qualified GitShell
import           Repo             (Repo)
import qualified Repo
import           System.Directory (createDirectoryIfMissing, doesFileExist,
                                   getCurrentDirectory, getDirectoryContents)
import           System.FilePath  (dropFileName, makeRelative, normalise,
                                   splitDirectories, takeBaseName, takeBaseName,
                                   takeExtension, takeFileName, (<.>), (</>))


-- Until this is done by gipeda, we have to also produce the backlog on our own.
writeBacklog :: Repo -> IO ()
writeBacklog repo = do
  projectDir <- Repo.projectDir repo
  path <- Repo.cloneDir repo
  backlog <- Repo.backlogFile repo
  hasClone <- GitShell.isRepositoryRoot path
  unfinished <- if not hasClone -- This should never be true, actually
    then return Set.empty
    else do
      allCommits <- GitShell.allCommits path
      resultsDir <- Repo.resultsDir repo
      createDirectoryIfMissing True resultsDir
      alreadyHandledCommits <- Set.fromList . map takeBaseName <$> getDirectoryContents resultsDir
      return (Set.difference allCommits alreadyHandledCommits)
  writeFile backlog (unlines (Set.toList unfinished))


readBacklog :: Repo -> IO (Set SHA)
readBacklog repo = do
  backlog <- Repo.backlogFile repo
  exists <- doesFileExist backlog
  if exists
    then Set.fromList . lines <$> readFile backlog
    else return Set.empty


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
    , matchProjectRelativeDirectory [] cwd path
    ]


writeBenchmarkCSV :: Repo -> SHA -> String -> IO ()
writeBenchmarkCSV repo commit result = do
  cwd <- getCurrentDirectory
  writeFile (cwd </> Repo.uniqueName repo </> "site" </> "out" </> "results" </> commit <.> "csv") result


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

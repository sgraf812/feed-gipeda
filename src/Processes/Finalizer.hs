module Processes.Finalizer
  ( finalizeAndProduceBacklog
  ) where


import qualified Control.Distributed.Process                as DP
import qualified Control.Distributed.Process.Extras.Time    as DPET
import qualified Control.Distributed.Process.ManagedProcess as DPM
import           Control.Monad                              (forever)
import           Data.Char                                  (toLower)
import           Data.List                                  (isSuffixOf)
import           Data.Map                                   (Map)
import qualified Data.Map                                   as Map
import           Data.Set                                   (Set)
import qualified Data.Set                                   as Set
import qualified GitShell
import           Processes.ConfigWatcher                    (RepoDiff)
import qualified Processes.ConfigWatcher                    as ConfigWatcher
import qualified Processes.FileWatcher                      as FileWatcher
import           Repo                                       (Repo)
import qualified Repo
import           System.Directory                           (getCurrentDirectory)
import           System.FilePath                            (dropFileName,
                                                             makeRelative,
                                                             normalise,
                                                             splitDirectories,
                                                             takeExtension,
                                                             (</>))
import qualified Worker


-- Until this is done by gipeda, we have to also produce the backlog on our own.
produceBacklog :: Repo -> IO ()
produceBacklog repo = do
  projectDir <- Repo.projectDir repo
  path <- Repo.cloneDir repo
  hasClone <- GitShell.isRepositoryRoot path
  unfinished <- if not hasClone -- This should never be true, actually
    then return Set.empty
    else do
      allCommits <- GitShell.allCommits path
      resultsDir <- Repo.resultsDir repo
      createDirectoryIfMissing True resultsDir
      alreadyHandledCommits <- Set.fromList . map takeBaseName <$> getDirectoryContents resultsDir
      return (Set.difference allCommits alreadyHandledCommits)
  writeFile (projectDir </> "backlog.txt") (unlines (Set.toList unfinished))


finalizeAndProduceBacklog :: FilePath -> Maybe String -> DPM.ControlChannel Repo -> DP.Process ()
finalizeAndProduceBacklog gipeda remoteDir inport = do
  cwd <- DP.liftIO getCurrentDirectory

  let
    splitNormalisedDirs :: FilePath -> [String]
    splitNormalisedDirs =
      splitDirectories . makeRelative cwd . normalise . dropFileName

    uniqueName :: FilePath -> String
    uniqueName =
      head . splitNormalisedDirs

    isBenchmarkCSV :: FilePath -> Bool
    isBenchmarkCSV path =
      and
        [ map toLower (takeExtension path) == "csv"
        , length (splitNormalisedDirs path) == 4
        , ["site", "out", "results"] `isSuffixOf` splitNormalisedDirs path
        ]

    impl :: Repo -> DP.Process ()
    impl repo = do
      Worker.regenerateAndDeploy gipeda remoteDir Set.empty repo
      produceBacklog repo

    lookupRepo :: String -> IO (Maybe Repo)
    lookupRepo uniqueName = do
      let path = cwd </> uniqueName </> "repository"
      hasClone <- GitShell.hasClone path
      if hasClone
        then GitShell.remoteRepo path
        else return Nothing


  pid <- DP.getSelfPid
  cc <- DPM.newControlChan
  DP.spawnLocal $ do
    DP.link pid
    cwd <- DP.liftIO getCurrentDirectory
    FileWatcher.watchTree cwd isBenchmarkCSV (DPM.channelControlPort cc)

  DPM.serve () (DPM.statelessInit DPET.Infinity) DPM.statelessProcess
    { DPM.apiHandlers =
        [ DPM.handleControlChan inport $ \_ repo -> do
            impl repo
            DPM.continue ()
        , DPM.handleControlChan cc $ \_ evt -> do
            -- the @eventPath@ satisfies @isBenchmarkCSV@, so using head is safe
            maybeRepo <- lookupRepo (uniqueName (FileWatcher.eventPath evt))
            case maybeRepo of
              Nothing -> DPM.continue ()
              Just repo -> do
                -- repos will not track deleted ones, so chances are that
                -- might kick off benchmarks for repos which shouldn't be watched
                -- anymore according to a config change.
                when (Set.member repo repos) (impl repo)
                DPM.continue ()
        ]
    }

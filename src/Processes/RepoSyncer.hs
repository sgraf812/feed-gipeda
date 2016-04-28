module Processes.RepoSyncer
  ( syncRepos
  ) where


import qualified Control.Distributed.Process                as DP
import qualified Control.Distributed.Process.Extras.Time    as DPET
import qualified Control.Distributed.Process.ManagedProcess as DPM
import           Control.Monad                              (forever)
import qualified GitShell
import           Processes.ConfigWatcher                    (RepoDiff)
import qualified Processes.ConfigWatcher                    as ConfigWatcher
import           Repo                                       (Repo)
import qualified Repo
import           System.Directory                           (createDirectoryIfMissing)


sync :: Repo -> IO ()
sync repo = do
  path <- Repo.cloneDir repo
  hasClone <- GitShell.isRepositoryRoot path
  if hasClone
    then GitShell.fetch path
    else do
      createDirectoryIfMissing True path
      GitShell.cloneBare repo path


syncRepos :: DPM.ControlChannel RepoDiff -> DPM.ControlPort Repo -> DP.Process ()
syncRepos inport outport = do
  (sendPort, recvPort) <- DP.newChan

  let
    worker :: DP.Process ()
    worker = forever $ do
      repo <- DP.receiveChan recvPort
      DP.liftIO (sync repo)
      DPM.sendControlMessage outport repo

  pid <- DP.getSelfPid
  DP.spawnLocal $ DP.link pid >> worker

  DPM.serve () (DPM.statelessInit DPET.Infinity) DPM.statelessProcess
    { DPM.apiHandlers =
        [ DPM.handleControlChan inport $ \_ diff -> do
            mapM_ (DP.sendChan sendPort) (ConfigWatcher.dirty diff)
            DPM.continue ()
        ]
    }

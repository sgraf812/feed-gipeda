{-# LANGUAGE DeriveGeneric #-}

module Processes.ConfigWatcher
  ( RepoDiff
  , dirty
  , removed
  , watchConfigFile
  , periodicallyMarkDirty
  ) where


import           Config                                     (Config)
import qualified Config
import           Control.Concurrent                         (threadDelay)
import qualified Control.Distributed.Process                as DP
import qualified Control.Distributed.Process.Extras.Time    as DPET
import qualified Control.Distributed.Process.ManagedProcess as DPM
import           Control.Monad                              (forever)
import           Data.Binary                                (Binary)
import           Data.Set                                   (Set)
import qualified Data.Set                                   as Set
import           Data.Time                                  (NominalDiffTime)
import qualified Data.Time                                  as Time
import           Data.Typeable                              (Typeable)
import           GHC.Generics                               (Generic)
import qualified Processes.FileWatcher                      as FileWatcher
import           Repo                                       (Repo)


data RepoDiff
  = RepoDiff
  { dirty   :: Set Repo
  , removed :: Set Repo
  } deriving (Eq, Show, Generic, Typeable)
instance Binary RepoDiff


watchConfigFile :: FilePath -> DPM.ControlPort RepoDiff -> DP.Process ()
watchConfigFile configFile outport = do
  pid <- DP.getSelfPid
  cc <- DPM.newControlChan

  DP.spawnLocal $ do
    DP.link pid
    FileWatcher.watchFile configFile (DPM.channelControlPort cc)

  DPM.serve () (\_ -> return (DPM.InitOk Set.empty DPET.Infinity)) DPM.defaultProcess
    { DPM.apiHandlers =
        [ DPM.handleControlChan cc $ \oldRepos evt ->
            case evt of
              FileWatcher.Removed _ -> DPM.continue oldRepos
              _ -> do
                result <- DP.liftIO (Config.decodeFile (FileWatcher.eventPath evt))
                either
                  (\e -> DP.say e >> DPM.continue oldRepos)
                  (\config -> do
                    DPM.sendControlMessage outport
                      (RepoDiff
                        (Config.repos config `Set.difference` oldRepos)
                        (oldRepos `Set.difference` Config.repos config))
                    DPM.continue (Config.repos config))
                  result
        ]
    }


periodicallyMarkDirty :: NominalDiffTime -> DPM.ControlChannel RepoDiff -> DPM.ControlPort RepoDiff -> DP.Process ()
periodicallyMarkDirty dt inport outport = do
  pid <- DP.getSelfPid
  cc <- DPM.newControlChan

  let
    waitThenMarkDirty :: DP.Process ()
    waitThenMarkDirty = do
      DP.liftIO (threadDelay (ceiling (dt*1000000)))
      DPM.sendControlMessage (DPM.channelControlPort cc) ()

  DP.spawnLocal $ DP.link pid >> forever waitThenMarkDirty

  DPM.serve () (\_ -> return (DPM.InitOk Set.empty DPET.Infinity)) DPM.defaultProcess
    { DPM.apiHandlers =
        [ DPM.handleControlChan inport $ \repos diff -> do
            DPM.sendControlMessage outport diff
            DPM.continue (Set.union (Set.difference repos (removed diff)) (dirty diff))
        , DPM.handleControlChan cc $ \repos () -> do
            DPM.sendControlMessage outport (RepoDiff repos Set.empty)
            DPM.continue repos
        ]
    }

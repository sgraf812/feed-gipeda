module Processes.Utils
  ( broadcast
  ) where


import qualified Control.Distributed.Process                as DP
import qualified Control.Distributed.Process.Extras.Time    as DPET
import qualified Control.Distributed.Process.ManagedProcess as DPM
import qualified Control.Distributed.Process.Serializable   as DPS


type ManagedProcess input output =
  DPM.ControlChannel input -> DPM.ControlPort output -> DP.Process ()


broadcast
  :: DPS.Serializable input
  => DPM.ControlChannel input
  -> [DPM.ControlPort input]
  -> DP.Process ()
broadcast input outputs =
  DPM.serve () (DPM.statelessInit DPET.Infinity) DPM.statelessProcess
    { DPM.apiHandlers =
        [ DPM.handleControlChan input $ \_ x -> do
            mapM_ (\o -> DPM.sendControlMessage o x) outputs
            DPM.continue ()
        ]
    }

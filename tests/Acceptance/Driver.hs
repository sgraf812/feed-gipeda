{-# LANGUAGE RecordWildCards #-}

module Acceptance.Driver
  ( Args (..)
  , withCheckInTmpDir
  , withOneShotInTmpDir
  , withDaemonInTmpDir
  , withMasterInTmpDir
  , withSlave
  ) where


import           Control.Exception          (bracket)
import           Control.Monad              (when)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Managed      (Managed, managed)
import           Control.Monad.Trans.Writer (Writer, execWriter, tell)
import           Data.Conduit               (Source, ($=), (=$=))
import qualified Data.Conduit.List          as CL
import           Data.Conduit.Process       (ClosedStream (..), CreateProcess,
                                             InputSource, OutputSink,
                                             StreamingProcessHandle,
                                             readCreateProcessWithExitCode,
                                             streamingProcess,
                                             streamingProcessHandleRaw,
                                             terminateProcess)
import           Data.Conduit.Text          (decode, utf8)
import           Data.Text                  (unpack)
import           System.Exit                (ExitCode)
import           System.IO.Temp             (withSystemTempDirectory)
import           System.Process             (cwd, proc, showCommandForUser)


data Args
  = Args
  { check         :: Bool
  , config        :: FilePath
  , deploymentDir :: Maybe FilePath
  , watch         :: Maybe Int
  , masterPort    :: Maybe Int
  } deriving (Show, Eq)


defaultConfig :: FilePath -> Args
defaultConfig config =
  Args False config Nothing Nothing Nothing


withCheckInTmpDir
  :: FilePath
  -> Managed (FilePath, Source IO String, Source IO String, StreamingProcessHandle)
withCheckInTmpDir config =
  withExecuteInTmpDir (defaultConfig config) { check = True }


withOneShotInTmpDir
  :: Maybe FilePath
  -> FilePath
  -> Managed (FilePath, Source IO String, Source IO String, StreamingProcessHandle)
withOneShotInTmpDir deploymentDir config =
  withExecuteInTmpDir (defaultConfig config)
    { deploymentDir = deploymentDir
    }


withDaemonInTmpDir
  :: Maybe FilePath
  -> Int
  -> FilePath
  -> Managed (FilePath, Source IO String, Source IO String, StreamingProcessHandle)
withDaemonInTmpDir deploymentDir dt config =
  withExecuteInTmpDir (defaultConfig config)
    { deploymentDir = deploymentDir
    , watch = Just dt
    }


withMasterInTmpDir
  :: Int
  -> FilePath
  -> Managed (FilePath, Source IO String, Source IO String, StreamingProcessHandle)
withMasterInTmpDir port config =
  withExecuteInTmpDir (defaultConfig config)
    { masterPort = Just port
    }


withSlave :: Int -> Managed (Source IO String, Source IO String, StreamingProcessHandle)
withSlave port =
  withProcess (proc "feed-gipeda" ["--slave", "localhost:" ++ show port])


withProcess :: CreateProcess -> Managed (Source IO String, Source IO String, StreamingProcessHandle)
withProcess cp = do
  (ClosedStream, stdout, stderr, handle) <- managed (bracket acquire release)
  return (stdout $= decodeString, stderr $= decodeString, handle)
    where
      acquire =
        streamingProcess cp
      release (_, _, _, h) =
        terminateProcess (streamingProcessHandleRaw h)
      decodeString =
        decode utf8 =$= CL.map unpack


withExecuteInTmpDir
  :: Args
  -> Managed (FilePath, Source IO String, Source IO String, StreamingProcessHandle)
withExecuteInTmpDir Args{..} = do
  let
    whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
    whenJust val action =
      maybe (return ()) action val

    args :: [String]
    args = execWriter $ do
      tell ["--config", config]
      when check (tell ["--check"])
      whenJust deploymentDir $ \r -> tell ["--deploy-to", r]
      whenJust watch $ \dt -> tell ["--watch", show dt]
      whenJust masterPort $ \p -> tell ["--master", "localhost:" ++ show p]
      return ()

  path <- managed (withSystemTempDirectory "feed-gipeda")
  (stdout, stderr, handle) <- withProcess (proc "feed-gipeda" args) { cwd = Just path }
  return (path, stdout, stderr, handle)

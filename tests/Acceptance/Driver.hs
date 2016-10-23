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
import           Data.ByteString            (ByteString)
import           Data.Conduit               (Source, ($=), (=$=))
import qualified Data.Conduit.List          as CL
import           Data.Conduit.Process       (ClosedStream (..), CreateProcess,
                                             InputSource, OutputSink,
                                             StreamingProcessHandle,
                                             readCreateProcessWithExitCode,
                                             streamingProcess,
                                             streamingProcessHandleRaw,
                                             terminateProcess,
                                             waitForStreamingProcess)
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
  -> Managed (FilePath, Source IO ByteString, Source IO ByteString, StreamingProcessHandle)
withCheckInTmpDir config =
  withExecuteInTmpDir (defaultConfig config) { check = True }


withOneShotInTmpDir
  :: Maybe FilePath
  -> FilePath
  -> Managed (FilePath, Source IO ByteString, Source IO ByteString, StreamingProcessHandle)
withOneShotInTmpDir deploymentDir config =
  withExecuteInTmpDir (defaultConfig config)
    { deploymentDir = deploymentDir
    }


withDaemonInTmpDir
  :: Maybe FilePath
  -> Int
  -> FilePath
  -> Managed (FilePath, Source IO ByteString, Source IO ByteString, StreamingProcessHandle)
withDaemonInTmpDir deploymentDir dt config =
  withExecuteInTmpDir (defaultConfig config)
    { deploymentDir = deploymentDir
    , watch = Just dt
    }


withMasterInTmpDir
  :: Int
  -> FilePath
  -> Managed (FilePath, Source IO ByteString, Source IO ByteString, StreamingProcessHandle)
withMasterInTmpDir port config =
  withExecuteInTmpDir (defaultConfig config)
    { masterPort = Just port
    }


withSlave :: Int -> Int -> Managed (Source IO ByteString, Source IO ByteString, StreamingProcessHandle)
withSlave mp sp =
  withProcess (proc "feed-gipeda" ["--slave", show sp ++ ":127.0.0.1:" ++ show mp])


withProcess :: CreateProcess -> Managed (Source IO ByteString, Source IO ByteString, StreamingProcessHandle)
withProcess cp = do
  (ClosedStream, stdout, stderr, handle) <- managed (bracket acquire release)
  return (stdout, stderr, handle)
    where
      acquire =
        streamingProcess cp
      release (_, _, _, h) =
        terminateProcess (streamingProcessHandleRaw h)


withExecuteInTmpDir
  :: Args
  -> Managed (FilePath, Source IO ByteString, Source IO ByteString, StreamingProcessHandle)
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
      whenJust masterPort $ \p -> tell ["--master", show p]
      return ()

  path <- managed (withSystemTempDirectory "feed-gipeda")
  (stdout, stderr, handle) <- withProcess (proc "feed-gipeda" args) { cwd = Just path }
  return (path, stdout, stderr, handle)

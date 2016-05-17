{-# LANGUAGE RecordWildCards #-}

module Acceptance.Driver
  ( Args (..)
  , withCheckInTmpDir
  , withOneShotInTmpDir
  , withDaemonInTmpDir
  , withMasterInTmpDir
  , slave
  ) where


import           Control.Monad              (when)
import           Control.Monad.Managed      (Managed, liftIO, managed)
import           Control.Monad.Trans.Writer (Writer, execWriter, tell)
import           System.Exit                (ExitCode)
import           System.IO.Temp             (withSystemTempDirectory)
import           System.Process             (cwd, proc,
                                             readCreateProcessWithExitCode,
                                             showCommandForUser)


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


withCheckInTmpDir :: FilePath -> Managed (FilePath, ExitCode, String, String)
withCheckInTmpDir config = do
  (path, fork) <- withExecuteInTmpDir (defaultConfig config) { check = True }
  (exitCode, stdout, stderr) <- liftIO fork
  return (path, exitCode, stdout, stderr)


withOneShotInTmpDir :: Maybe FilePath -> FilePath -> Managed (FilePath, ExitCode, String, String)
withOneShotInTmpDir deploymentDir config = do
  (path, fork) <- withExecuteInTmpDir (defaultConfig config)
    { deploymentDir = deploymentDir
    }
  (exitCode, stdout, stderr) <- liftIO fork
  return (path, exitCode, stdout, stderr)


withDaemonInTmpDir :: Maybe FilePath -> Int -> FilePath -> Managed (FilePath, IO (ExitCode, String, String))
withDaemonInTmpDir deploymentDir dt config =
  withExecuteInTmpDir (defaultConfig config)
    { deploymentDir = deploymentDir
    , watch = Just dt
    }


withMasterInTmpDir :: Int -> FilePath -> Managed (FilePath, IO (ExitCode, String, String))
withMasterInTmpDir port config =
  withExecuteInTmpDir (defaultConfig config)
    { masterPort = Just port
    }


slave :: Int -> IO (ExitCode, String, String)
slave port =
  readCreateProcessWithExitCode (proc "feed-gipeda" ["--slave", "localhost:" ++ show port]) ""


withExecuteInTmpDir :: Args -> Managed (FilePath, IO (ExitCode, String, String))
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
  return (path, liftIO $ readCreateProcessWithExitCode (proc "feed-gipeda" args) { cwd = Just path } "")

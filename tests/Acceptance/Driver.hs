{-# LANGUAGE RecordWildCards #-}

module Acceptance.Driver
  ( Args (..)
  , withCheckInTmpDir
  , withOneShotInTmpDir
  , withDaemonInTmpDir
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
  , oneShot       :: Bool
  , dt            :: Maybe Int
  } deriving (Show, Eq)


defaultConfig :: FilePath -> Args
defaultConfig config =
  Args False config Nothing False Nothing


withCheckInTmpDir :: FilePath -> Managed (FilePath, ExitCode, String, String)
withCheckInTmpDir config = do
  (path, fork) <- withExecuteInTmpDir (defaultConfig config) { check = True }
  (exitCode, stdout, stderr) <- liftIO fork
  return (path, exitCode, stdout, stderr)


withOneShotInTmpDir :: Maybe FilePath -> FilePath -> Managed (FilePath, ExitCode, String, String)
withOneShotInTmpDir deploymentDir config = do
  (path, fork) <- withExecuteInTmpDir (defaultConfig config)
    { deploymentDir = deploymentDir
    , oneShot = True
    }
  (exitCode, stdout, stderr) <- liftIO fork
  return (path, exitCode, stdout, stderr)


withDaemonInTmpDir :: Maybe FilePath -> Maybe Int -> FilePath -> Managed (FilePath, IO (ExitCode, String, String))
withDaemonInTmpDir deploymentDir dt config =
  withExecuteInTmpDir (defaultConfig config)
    { deploymentDir = deploymentDir
    , dt = dt
    }


withExecuteInTmpDir :: Args -> Managed (FilePath, IO (ExitCode, String, String))
withExecuteInTmpDir Args{..} = do
  let
    args :: [String]
    args = execWriter $ do
      tell ["--config", config]
      when check (tell ["--check"])
      when oneShot (tell ["--one-shot"])
      maybe (return ()) (\r -> tell ["--rsync", r]) deploymentDir
      maybe (return ()) (\dt -> tell ["--dt", show dt]) dt
      return ()

  path <- managed (withSystemTempDirectory "feed-gipeda")
  return (path, liftIO $ readCreateProcessWithExitCode (proc "feed-gipeda" args) { cwd = Just path } "")

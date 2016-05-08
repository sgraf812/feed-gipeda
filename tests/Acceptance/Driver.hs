{-# LANGUAGE RecordWildCards #-}

module Acceptance.Driver
  ( Args (..)
  , defaultConfig
  , withCheckInTmpDir
  , withOneShotInTmpDir
  ) where


import           Control.Monad              (when)
import           Control.Monad.Managed      (Managed, liftIO, managed)
import           Control.Monad.Trans.Writer (Writer, execWriter, tell)
import           System.Exit                (ExitCode)
import           System.IO.Temp             (withSystemTempDirectory)
import           System.Process             (cwd, proc,
                                             readCreateProcessWithExitCode)


data Args
  = Args
  { check         :: Bool
  , config        :: FilePath
  , deploymentDir :: Maybe FilePath
  , oneShot       :: Bool
  } deriving (Show, Eq)


defaultConfig :: FilePath -> Args
defaultConfig config =
  Args False config Nothing False


withCheckInTmpDir :: FilePath -> Managed (FilePath, ExitCode, String, String)
withCheckInTmpDir config =
  withExecuteInTmpDir (defaultConfig config) { check = True }


withOneShotInTmpDir :: Maybe FilePath -> FilePath -> Managed (FilePath, ExitCode, String, String)
withOneShotInTmpDir deploymentDir config =
  withExecuteInTmpDir (defaultConfig config)
    { deploymentDir = deploymentDir
    , oneShot = True
    }


withExecuteInTmpDir :: Args -> Managed (FilePath, ExitCode, String, String)
withExecuteInTmpDir Args{..} = do
  let
    args :: [String]
    args = execWriter $ do
      tell ["--config", config]
      when check (tell ["--check"])
      when oneShot (tell ["--one-shot"])
      maybe (return ()) (\r -> tell ["--rsync", r]) deploymentDir
      return ()

  path <- managed (withSystemTempDirectory "feed-gipeda")
  (c, out, err) <- liftIO $
    readCreateProcessWithExitCode (proc "feed-gipeda" args) { cwd = Just path } ""
  return (path, c, out, err)

{-# LANGUAGE RecordWildCards #-}

module Acceptance.Driver
  ( Args (..)
  , defaultConfig
  , withExecuteInTmpDir
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
  { check  :: Bool
  , config :: FilePath
  } deriving (Show, Eq)


defaultConfig :: FilePath -> Args
defaultConfig =
  Args False


withExecuteInTmpDir :: Args -> Managed (FilePath, ExitCode, String, String)
withExecuteInTmpDir Args{..} = do
  let
    args :: [String]
    args = execWriter $ do
      tell ["--config", config]
      when check (tell ["--check"])
      return ()

  path <- managed (withSystemTempDirectory "feed-gipeda")
  (c, out, err) <- liftIO $
    readCreateProcessWithExitCode (proc "feed-gipeda" args) { cwd = Just path } ""
  return (path, c, out, err)

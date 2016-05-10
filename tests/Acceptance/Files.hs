{-# LANGUAGE TemplateHaskell #-}

module Acceptance.Files
  ( withWellFormedConfig
  , withMalformedConfig
  , wellFormedConfig
  , withInitGitRepo
  , makeCloneOf
  ) where


import           Control.Monad.Managed (Managed, liftIO, managed)
import           Data.ByteString       (ByteString, hPutStr)
import           Data.FileEmbed        (embedFile)
import           Network.URI           (URI, uriToString)
import           System.IO             (hClose)
import           System.IO.Temp        (withSystemTempDirectory,
                                        withSystemTempFile)
import           System.Process        (proc, readCreateProcessWithExitCode)

wellFormedConfig :: ByteString
wellFormedConfig =
  $(embedFile "tests/Acceptance/Files/feed-gipeda.yaml")


malformedConfig :: ByteString
malformedConfig =
  $(embedFile "tests/Acceptance/Files/malformed-feed-gipeda.yaml")


withTempFileFromByteString :: ByteString -> Managed FilePath
withTempFileFromByteString bytes = do
  (path, handle) <- managed (withSystemTempFile "feed-gipeda.yaml" . curry)
  liftIO (hPutStr handle bytes)
  liftIO (hClose handle)
  return path


withWellFormedConfig :: Managed FilePath
withWellFormedConfig =
  withTempFileFromByteString wellFormedConfig


withMalformedConfig :: Managed FilePath
withMalformedConfig =
  withTempFileFromByteString malformedConfig


withInitGitRepo :: Managed FilePath
withInitGitRepo = do
  path <- managed (withSystemTempDirectory "feed-gipeda")
  liftIO $ readCreateProcessWithExitCode (proc "git" ["-C", path, "init"]) ""
  return path


makeCloneOf :: FilePath -> URI -> IO ()
makeCloneOf path remote = do
  readCreateProcessWithExitCode (proc "git" ["-C", path, "remote", "add", "origin", uriToString id remote ""]) ""
  readCreateProcessWithExitCode (proc "git" ["-C", path, "pull", "origin", "master"]) ""
  return ()

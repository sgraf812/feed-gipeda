{-# LANGUAGE TemplateHaskell #-}

module Acceptance.Files
  ( withWellFormedConfig
  , withMalformedConfig
  , wellFormedConfig
  ) where


import           Control.Monad.Managed (Managed, liftIO, managed)
import           Data.ByteString       (ByteString, hPutStr)
import           Data.FileEmbed        (embedFile)
import           System.IO             (hClose)
import           System.IO.Temp        (withSystemTempFile)

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

module FeedGipeda.Prelude
  ( logInfo
  , logDebug
  , logWarn
  , logError
  , readFileMaybe
  ) where


import qualified Control.Logging        as Logging
import           Control.Monad.IO.Class
import qualified Data.Text              as Text
import           System.Directory       (doesFileExist)


logInfo :: MonadIO io => String -> io ()
logInfo = liftIO . Logging.log . Text.pack


logDebug :: MonadIO io => String -> io ()
logDebug = liftIO . Logging.debug . Text.pack


logWarn :: MonadIO io => String -> io ()
logWarn = liftIO . Logging.warn . Text.pack


logError :: String -> a
logError = Logging.errorL . Text.pack


readFileMaybe :: MonadIO io => FilePath -> io (Maybe String)
readFileMaybe file = liftIO $ do
  exists <- doesFileExist file
  if exists
    then Just <$> readFile file
    else return Nothing

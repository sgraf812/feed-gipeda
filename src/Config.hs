{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config (..)
  , withWatchFile
  , checkFile
  ) where


import           Control.Monad   (void)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Yaml       (FromJSON (..), Value (Object), (.:))
import qualified Data.Yaml       as Yaml
import           Network.URI     (parseURI)
import           Repo            (Repo (..))
import qualified Repo
import           System.FilePath (dropFileName, equalFilePath)
import qualified System.FSNotify as FS


data Config
  = Config
  { repos :: Set Repo
  }


instance FromJSON Repo where
  parseJSON v = do
    s <- parseJSON v
    uri <- maybe (fail "Could not parse URI") return (parseURI s)
    return (Repo uri)


instance FromJSON Config where
  parseJSON (Object v) =
    fmap (Config . Set.fromList) (v .: "repositories")
  parseJSON _ =
    fail "Object"


checkFile :: FilePath -> IO (Maybe String)
checkFile file =
  fmap
    (either
      (Just . Yaml.prettyPrintParseException)
      (\Config{} -> Nothing))
    (Yaml.decodeFileEither file)


decodeFileAndNotify :: (Config -> IO a) -> FilePath -> IO ()
decodeFileAndNotify onNewConfig file =
  Yaml.decodeFileEither file >>= either
    (\e -> do
      putStrLn "Could not decode the config file:"
      putStrLn (Yaml.prettyPrintParseException e))
    (void . onNewConfig)


withWatchFile :: FilePath -> (Config -> IO a) -> IO b -> IO b
withWatchFile file onNewConfig inner = do
  let
    filterEvents evt =
      case evt of
        FS.Removed _ _ -> False
        _ -> equalFilePath file (FS.eventPath evt)

  FS.withManager $ \mgr -> do
    FS.watchDir
      mgr
      (dropFileName file)
      filterEvents
      (decodeFileAndNotify onNewConfig . FS.eventPath)
    inner

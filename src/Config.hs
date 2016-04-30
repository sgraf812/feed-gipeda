{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config
  , empty
  , repos
  , decodeFile
  , checkFile
  ) where


import           Control.Arrow          (left)
import           Control.Monad          (forever)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Yaml              (FromJSON (..), Value (Object), (.:))
import qualified Data.Yaml              as Yaml
import           Network.URI            (parseURI)
import           Repo                   (Repo (..))
import qualified Repo
import           System.FilePath        (dropFileName, equalFilePath)


data Config
  = Config
  { repos :: Set Repo
  } deriving (Eq, Show)


empty :: Config
empty =
  Config Set.empty


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


decodeFile :: FilePath -> IO (Either String Config)
decodeFile file = fmap (left errorToString) (Yaml.decodeFileEither file)
  where
    errorToString :: Yaml.ParseException -> String
    errorToString e =
      unlines
        [ "Could not decode the config file:"
        , Yaml.prettyPrintParseException e
        ]


checkFile :: FilePath -> IO (Maybe String)
checkFile file =
  fmap (either Just (const Nothing)) (decodeFile file)

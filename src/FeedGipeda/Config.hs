{-# LANGUAGE OverloadedStrings #-}

{-| @feed-gipeda.yaml@ related stuff like decoding and syntax checking.
-}


module FeedGipeda.Config
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
import           FeedGipeda.Repo        (Repo (..))
import qualified FeedGipeda.Repo        as Repo
import           Network.URI            (parseURI)
import           System.FilePath        (dropFileName, equalFilePath)


{-| Represents a decoded config file. Has the appropriate @FromJSON@ config to
    be deserializable from YAML.
-}
data Config
  = Config
  { repos :: Set Repo
  } deriving (Eq, Show)


-- | A config with no repositories to watch.
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


{-| @decodeFile file@ @Either@ decodes a @Config@ from a YAML @file@
    (such as @feed-gipeda.yaml@) or returns a parse error message.
-}
decodeFile :: FilePath -> IO (Either String Config)
decodeFile file = fmap (left errorToString) (Yaml.decodeFileEither file)
  where
    errorToString :: Yaml.ParseException -> String
    errorToString e =
      unlines
        [ "Could not decode the config file:"
        , Yaml.prettyPrintParseException e
        ]


{-| Checks for syntax errors while reading a @Config@ from the supplied YAML
    file.
-}
checkFile :: FilePath -> IO (Maybe String)
checkFile file =
  fmap (either Just (const Nothing)) (decodeFile file)

{-# LANGUAGE OverloadedStrings #-}

module Repo
  ( Repo()
  , uri
  , Repos
  , noRepos
  , toList
  , difference
  , uniqueName
  ) where

import qualified Data.Hash as Hash
import Data.Word (Word64)
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Data.Yaml               (FromJSON (..), Value (Object), (.:))
import           Network.URI
import           System.FilePath         (takeFileName)

newtype Repo = Repo
  { unRepo :: URI
  } deriving (Eq, Ord, Show)


uri :: Repo -> String
uri (Repo u) =
  uriToString id u ""


newtype Repos = Repos
  { unRepos :: Set Repo
  } deriving (Show)


noRepos :: Repos
noRepos =
  Repos Set.empty


toList :: Repos -> [Repo]
toList =
  Set.toList . unRepos


difference :: Repos -> Repos -> Repos
difference (Repos a) (Repos b) =
  Repos (Set.difference a b)


instance FromJSON Repo where
  parseJSON v = do
    s <- parseJSON v
    uri <- maybe (fail "Could not parse URI") return (parseURI s)
    return (Repo uri)


hash :: Repo -> Word64
hash =
  Hash.asWord64 . Hash.hash . show . uri


uniqueName :: Repo -> String
uniqueName repo =
  takeFileName (uriPath (unRepo repo)) ++ "-" ++ (show . hash) repo


instance FromJSON Repos where
  parseJSON (Object v) =
    fmap (Repos . Set.fromList) (v .: "repositories")
  parseJSON _ =
    fail "Object"

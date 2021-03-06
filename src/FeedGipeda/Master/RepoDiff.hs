-- | Provides means to diff sets of repositories.

module FeedGipeda.Master.RepoDiff
  ( RepoDiff (..)
  , compute
  , apply
  ) where


import           Data.Set        (Set)
import qualified Data.Set        as Set
import           FeedGipeda.Repo (Repo)


data RepoDiff
  = RepoDiff
  { added   :: Set Repo
  , removed :: Set Repo
  } deriving (Eq, Show)


compute :: Set Repo -> Set Repo -> RepoDiff
compute old new =
  RepoDiff (Set.difference new old) (Set.difference old new)


apply :: RepoDiff -> Set Repo -> Set Repo
apply diff =
  flip Set.difference (removed diff) . Set.union (added diff)

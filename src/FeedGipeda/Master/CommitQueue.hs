module FeedGipeda.Master.CommitQueue
  ( CommitQueue
  , new
  , dequeue
  , updateRepoBacklog
  ) where


import           Control.Concurrent.Event (Event)
import qualified Control.Concurrent.Event as Event
import           Control.Concurrent.MVar
import qualified Control.Logging          as Logging
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import qualified Data.Text                as Text
import qualified FeedGipeda.Gipeda        as Gipeda
import           FeedGipeda.GitShell      (SHA)
import qualified FeedGipeda.Master.File   as File
import           FeedGipeda.Prelude
import           FeedGipeda.Repo          (Repo)
import qualified FeedGipeda.Repo          as Repo


data CommitQueue
  = CommitQueue (MVar State) Event


data Backlog
  = Backlog
  { queue     :: [SHA]
  , set       :: Set SHA
  , blacklist :: Set SHA
  }


data State
  = State
  { backlogs :: Map Repo Backlog
  , lastRepo :: Maybe Repo
  }


new :: IO CommitQueue
new = do
  state <- newMVar (State Map.empty Nothing)
  stateChangedEvt <- Event.new
  return (CommitQueue state stateChangedEvt)


{-| Selects the next (Repo, Commit) pair approx. based on round-robin.
    Skips blacklisted commits, e.g. those which already have been selected
    previously, but still appear in backlogs.
-}
nextCommitView :: State -> Maybe ((Repo, SHA), State)
nextCommitView s = go traversal
  where
    -- We traverse the backlogs map starting from lastRepo
    -- and wrapping around after that. This seems to be the most
    -- elegant way to do that.
    traversal :: [Map Repo Backlog]
    traversal =
      maybe [backlogs s] (\repo -> toList (swap (splitLE repo (backlogs s)))) (lastRepo s)

    splitLE :: Ord k => k -> Map k v -> (Map k v, Map k v)
    splitLE k = readd . Map.splitLookup k
      where
        readd (le, Nothing, gr) = (le, gr)
        readd (le, Just eq, gr) = (Map.insert k eq le, gr)

    toList :: (a, a) -> [a]
    toList (a, b) = [a, b]

    swap :: (a, b) -> (b, a)
    swap (a, b) = (b, a)

    go :: [Map Repo Backlog] -> Maybe ((Repo, SHA), State)
    go [] = Nothing
    go (m : ms) =
      case Map.minViewWithKey m of
        Nothing -> go ms
        Just ((repo, Backlog queue set blacklist), m') ->
          case filter (not . flip Set.member blacklist) queue of
            [] -> go (m' : ms)
            commit : _ -> Just ((repo, commit), newState)
              where
                newState = State
                  { lastRepo = Just repo
                  , backlogs = Map.insert repo newBacklog (backlogs s)
                  }
                newBacklog = Backlog queue set (Set.insert commit blacklist)


dequeue :: CommitQueue -> IO (Repo, SHA)
dequeue cq@(CommitQueue stateVar stateChanged) = do
  Event.wait stateChanged
  maybePair <- modifyMVar stateVar $ \state ->
    case nextCommitView state of
      Nothing -> Event.clear stateChanged >> return (state, Nothing)
      Just (pair, newState) -> return (newState, Just (pair, newState))
  case maybePair of
    Nothing -> dequeue cq
    Just ((repo, commit), newState) -> do
      logInfo (unlines ["Dequeue (" ++ Repo.shortName repo ++ ", " ++ take 7 commit ++ "). New state: ", showState newState])
      return (repo, commit)


updateRepoBacklog :: CommitQueue -> Repo -> [SHA] -> IO Bool
updateRepoBacklog (CommitQueue stateVar stateChanged) repo backlog = do
  let backlogSet = Set.fromList backlog
  s <- backlogSet `seq` modifyMVar stateVar $ \s -> do
    let alterBacklog bl =
          if null backlog
            then Nothing
            else Just Backlog
              { queue = backlog
              , set = backlogSet
              , blacklist = Set.intersection backlogSet (maybe Set.empty blacklist bl)
              }
        newState = s { backlogs = Map.alter alterBacklog repo (backlogs s) }
    Event.set stateChanged
    return (newState, newState)
  logInfo (unlines ["Updated the commit queue. New state:", showState s])
  return (Map.null (backlogs s))


showState :: State -> String
showState (State backlogs lastRepo) =
  unlines $
    [ "lastRepo: " ++ show (Repo.shortName <$> lastRepo)
    , "backlogs: "
    ] ++ if Map.null backlogs
          then ["empty!"]
          else Map.foldrWithKey mkEntry [] backlogs
    where
      mkEntry repo (Backlog queue set bl) rest =
        ("  "
          ++ Repo.shortName repo ++ " -- "
          ++ "next: " ++ (show . safeHead . map (take 7) . filter (not . (`Set.member` bl))) queue
          ++ queueMsg queue ++ (show . map (take 7) . take 3) queue) : rest

      queueMsg queue =
        if length queue < 3
          then ", queue: "
          else ", first 3 in queue (" ++ show (length queue - 3) ++ " more): "

      safeHead [] = Nothing
      safeHead (x:xs) = Just x

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}

module Unit.CommitQueue
  ( tests
  ) where


import           Control.Concurrent            (forkIO, threadDelay)
import qualified Control.Logging               as Logging
import           Control.Monad.Free
import           Control.Monad.IO.Class
import           FeedGipeda.GitShell           (SHA)
import           FeedGipeda.Master.CommitQueue (CommitQueue)
import qualified FeedGipeda.Master.CommitQueue as CommitQueue
import           FeedGipeda.Repo               (Repo (..))
import qualified FeedGipeda.Repo               as Repo
import           System.Timeout                (timeout)
import           Test.Tasty
import           Test.Tasty.HUnit


data QueueF a
  = Update Repo [SHA] (Bool -> a)
  | TryDequeue (Maybe (Repo, SHA) -> a)
  | forall r. EmbedIO (IO r) (r -> a)


instance Functor QueueF where
  fmap f (Update repo commits g) = Update repo commits (f . g)
  fmap f (TryDequeue g) = TryDequeue (f . g)
  fmap f (EmbedIO action g) = EmbedIO action (f . g)


type Queue
  = Free QueueF


instance MonadIO (Free QueueF) where
  liftIO action = liftF (EmbedIO action id)


update :: Repo -> [SHA] -> Queue Bool
update repo commits = liftF (Update repo commits id)


tryDequeue :: Queue (Maybe (Repo, SHA))
tryDequeue = liftF (TryDequeue id)


timeoutDelay :: Int
timeoutDelay = 1000 -- 1 ms


runQueue :: CommitQueue -> Queue r -> IO r
runQueue queue = iterM run
  where
    run (Update repo commits k) = CommitQueue.updateRepoBacklog queue repo commits >>= k
    run (TryDequeue k) = timeout timeoutDelay (CommitQueue.dequeue queue) >>= k
    run (EmbedIO action k) = action >>= k


runEmptyQueue :: (CommitQueue -> Queue r) -> IO r
runEmptyQueue st = Logging.withStdoutLogging $ do
  Logging.setLogLevel Logging.LevelError
  q <- CommitQueue.new
  runQueue q (st q)


pipes, conduit, servant :: Repo
pipes = Repo.unsafeFromString "https://github.com/Gabriel439/Haskell-Pipes-Library"
conduit = Repo.unsafeFromString "https://github.com/snoyberg/conduit"
servant = Repo.unsafeFromString "https://github.com/haskell-servant/servant"


assertJust :: String -> Maybe a -> IO a
assertJust _ (Just a) = return a
assertJust msg _ = assertFailure msg >> error "Should never hit this"


assertNothing :: String -> Maybe a -> IO ()
assertNothing _ Nothing = return ()
assertNothing msg _ = assertFailure msg


assertDequeuedCommitFromRepo :: Repo -> SHA -> Queue ()
assertDequeuedCommitFromRepo repo commit = do
  (actualRepo, actualCommit) <- tryDequeue >>= liftIO . assertJust "dequeue should not time out"
  liftIO $ assertEqual "dequeued from wrong repo" repo actualRepo
  liftIO $ assertEqual "dequeued wrong commit" commit actualCommit


updateNonEmpty :: Repo -> [SHA] -> Queue ()
updateNonEmpty repo commits =
  update repo commits >>= liftIO . assertBool "queue was empty" . not


updateEmpty :: Repo -> Queue ()
updateEmpty repo =
  update repo [] >>= liftIO . assertBool "queue was not empty"


tests :: TestTree
tests = testGroup "CommitQueue"
  [ testGroup "single repo"
      [ testCase "update then dequeue and empty queue" $ runEmptyQueue $ \_ -> do
          updateNonEmpty pipes ["1", "3", "2"]
          assertDequeuedCommitFromRepo pipes "1"
          assertDequeuedCommitFromRepo pipes "3"
          assertDequeuedCommitFromRepo pipes "2"
          tryDequeue >>= liftIO . assertNothing "empty queue should lead to blocking"
          -- Even if we there aren't any more items to dequeue, the queue might not
          -- not be empty, since items are just blacklisted, meaning they are executing
          -- right now. We have to clear all items for that repo to effectively have
          -- an empty CommitQueue.
          updateEmpty pipes
      , testCase "concurrent update leads to unblocking of the dequeuer" $ runEmptyQueue $ \q -> do
          liftIO . forkIO . runQueue q $ updateNonEmpty pipes ["1"]
          assertDequeuedCommitFromRepo pipes "1"
      ]
  , testGroup "multiple repos"
      [ testCase "fairness" $ runEmptyQueue $ \_ -> do
          updateNonEmpty pipes ["1", "2"]
          assertDequeuedCommitFromRepo pipes "1"
          updateNonEmpty conduit ["4", "3"]
          -- we point now to what comes after pipes entry, so we wrap around
          assertDequeuedCommitFromRepo conduit "4"
          updateNonEmpty servant ["7", "5", "6"]
          -- lastRepo was conduit, so we continue with pipes
          assertDequeuedCommitFromRepo pipes "2"
          updateNonEmpty pipes ["8", "9"]
          -- pipes -> servant
          assertDequeuedCommitFromRepo servant "7"
          -- servant -> conduit
          assertDequeuedCommitFromRepo conduit "3"
          -- conduit -> pipes
          assertDequeuedCommitFromRepo pipes "8"
          -- pipes -> servant
          assertDequeuedCommitFromRepo servant "5"
          -- servant -> conduit, which is depleted, so -> pipes
          assertDequeuedCommitFromRepo pipes "9"
          -- pipes -> servant
          assertDequeuedCommitFromRepo servant "6"
          -- that should be all, see if dequeue blocks
          tryDequeue >>= liftIO . assertNothing "empty queue should lead to blocking"
          -- now tear down the queue
          updateNonEmpty conduit []
          updateNonEmpty pipes []
          updateEmpty servant
      ]
  ]

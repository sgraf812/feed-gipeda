{-# LANGUAGE ScopedTypeVariables #-}

{-| A @ConcurrentQueueSet@. Essentially a @Chan@ where an item is not
    enqueued if already present somewhere in the queue. Useful if the worker
    is idempotent and new (pot. duplicated) items generated faster than executed.
-}

module ConcurrentQueueSet
  ( ConcurrentQueueSet ()
  , empty
  , enqueue
  , dequeue
  ) where


import           Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import           Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import           Data.Set                (Set)
import qualified Data.Set                as Set


data ConcurrentQueueSet a = ConcurrentQueueSet
  { queue :: Chan a
  , mvar  :: MVar (Set a)
  }


empty :: IO (ConcurrentQueueSet a)
empty = ConcurrentQueueSet <$> newChan <*> newMVar Set.empty


enqueue :: Ord a => ConcurrentQueueSet a -> a -> IO ()
enqueue (ConcurrentQueueSet queue mvar) x =
  modifyMVar_ mvar impl
    where
      impl set =
        if Set.member x set
          then return set
          else do
            writeChan queue x
            return (Set.insert x set)


dequeue :: Ord a => ConcurrentQueueSet a -> IO a
dequeue (ConcurrentQueueSet queue mvar) = do
  x <- readChan queue
  modifyMVar_ mvar (return . Set.delete x)
  return x

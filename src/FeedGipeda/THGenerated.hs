{-# LANGUAGE TemplateHaskell #-}

{-| Contains TemplateHaskell stuff I don't want to recompile every time I make
    changes to other files, a pre-compiled header, so to say. Don't know if that
    even works.
-}

module FeedGipeda.THGenerated
  ( benchmarkClosure
  , stringDict
  , __remoteTable
  ) where

import           Control.Distributed.Process         (Closure, Process, Static,
                                                      liftIO)
import           Control.Distributed.Process.Closure (SerializableDict,
                                                      functionTDict, mkClosure,
                                                      remotable)
import           FeedGipeda.GitShell                 (SHA)
import           FeedGipeda.Repo                     (Repo)
import qualified FeedGipeda.Slave                    as Slave
import           FeedGipeda.Types                    (Timeout)


benchmarkProcess :: (String, Repo, SHA, Rational) -> Process String
benchmarkProcess (benchmarkScript, repo, sha, timeout) =
  liftIO (Slave.benchmark benchmarkScript repo sha (fromRational timeout))
remotable ['benchmarkProcess]


benchmarkClosure :: String -> Repo -> SHA -> Timeout -> Closure (Process String)
benchmarkClosure benchmarkScript repo commit timeout =
  $(mkClosure 'benchmarkProcess) (benchmarkScript, repo, commit, toRational timeout)


stringDict :: Static (SerializableDict String)
stringDict =
  $(functionTDict 'benchmarkProcess)

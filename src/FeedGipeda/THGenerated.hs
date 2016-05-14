{-# LANGUAGE TemplateHaskell #-}

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


benchmarkProcess :: (String, Repo, SHA) -> Process String
benchmarkProcess (benchmarkScript, repo, sha) =
  liftIO (Slave.benchmark benchmarkScript repo sha)
remotable ['benchmarkProcess]


benchmarkClosure :: String -> Repo -> SHA -> Closure (Process String)
benchmarkClosure benchmarkScript repo commit =
  $(mkClosure 'benchmarkProcess) (benchmarkScript, repo, commit)


stringDict :: Static (SerializableDict String)
stringDict =
  $(functionTDict 'benchmarkProcess)

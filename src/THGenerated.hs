{-# LANGUAGE TemplateHaskell #-}

module THGenerated
  ( benchmarkClosure
  , stringDict
  , __remoteTable
  ) where

import           Control.Distributed.Process         (Closure, Process, Static,
                                                      liftIO)
import           Control.Distributed.Process.Closure (SerializableDict,
                                                      functionTDict, mkClosure,
                                                      remotable)
import           GitShell                            (SHA)
import           Repo                                (Repo)
import qualified Worker


benchmarkProcess :: (FilePath, Repo, SHA) -> Process String
benchmarkProcess (cloben, repo, sha) =
  liftIO (Worker.benchmark cloben repo sha)
remotable ['benchmarkProcess]


benchmarkClosure :: (FilePath, Repo, SHA) -> Closure (Process String)
benchmarkClosure =
  $(mkClosure 'benchmarkProcess)


stringDict :: Static (SerializableDict String)
stringDict =
  $(functionTDict 'benchmarkProcess)

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


benchmarkProcess :: (Repo, SHA) -> Process String
benchmarkProcess (repo, sha) =
  liftIO (Worker.benchmark repo sha)
remotable ['benchmarkProcess]


benchmarkClosure :: Repo -> SHA -> Closure (Process String)
benchmarkClosure repo commit =
  $(mkClosure 'benchmarkProcess) (repo, commit)


stringDict :: Static (SerializableDict String)
stringDict =
  $(functionTDict 'benchmarkProcess)

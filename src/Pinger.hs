{-# LANGUAGE TemplateHaskell #-}

module Pinger
  ( main,
  )
where

import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Control.Monad (forever)
import Network.Transport.TCP (createTransport, defaultTCPAddr, defaultTCPParameters)

sampleTask :: (Int, String) -> Process ()
sampleTask (t, s) = liftIO (threadDelay (t * 1000000)) >> say s

remotable ['sampleTask]

myRemoteTable :: RemoteTable
myRemoteTable = Pinger.__remoteTable initRemoteTable

main :: IO ()
main = do
  Right transport <- createTransport (defaultTCPAddr "127.0.0.1" "10501") defaultTCPParameters
  node <- newLocalNode transport myRemoteTable
  runProcess node $ do
    us <- getSelfNode
    _ <- spawnLocal $ sampleTask (1 :: Int, "using spawnLocal" :: String)
    pid <- spawn us $ $(mkClosure 'sampleTask) (1 :: Int, "using spawn" :: String)
    liftIO $ threadDelay 2000000

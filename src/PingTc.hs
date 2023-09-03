{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module PingTc
  ( main,
  --    main2,
  --   masterMain,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Control.Monad
import Data.Binary
import Data.ByteString.Char8 (pack)
import Data.Typeable
import GHC.Generics (Generic)
import Network.Transport (EndPointAddress (..))
import Network.Transport.TCP

-- <<Message
data Message = Ping (SendPort ProcessId)
  deriving (Typeable, Generic)

instance Binary Message

-- >>

-- <<pingServer
pingServer :: Process ()
pingServer = do
  say $ "waiting ping"
  Ping chan <- expect
  say $ "ping received from %s" ++ (show chan)
  mypid <- getSelfPid
  sendChan chan mypid

-- >>

-- <<remotable
remotable ['pingServer]

-- >>

-- <<master
master :: [NodeId] -> Process ()
master peers = do
  ps <- forM peers $ \nid -> do
    say $ "spawning on " ++ (show nid)
    spawn nid $(mkStaticClosure 'pingServer)

  mapM_ monitor ps

  mypid <- getSelfPid

  ports <- forM ps $ \pid -> do
    say $ "pinging " ++ (show pid)
    (sendport, recvport) <- newChan
    send pid (Ping sendport)
    return recvport

  let loop [] = return ()
      loop (port : ps) = do
        receiveWait
          [ match $ \(ProcessMonitorNotification ref pid reason) -> do
              say (show pid ++ " died: " ++ show reason)
              loop (port : ps),
            matchChan port $ \p -> do
              say "pong on channel"
              loop ps
          ]
  loop ports

  say "All pongs successfully received"
  terminate

-- >>
myRemoteTable :: RemoteTable
myRemoteTable = __remoteTable initRemoteTable

-- <<main
main :: IO ()
main = do
  eitherTransport <- createTransport (defaultTCPAddr "127.0.0.1" "8080") defaultTCPParameters
  case eitherTransport of
    Left err -> print err -- Handle the error appropriately
    Right transport -> do
      localNode <- newLocalNode transport myRemoteTable
      let nodeId1 = NodeId . EndPointAddress $ "127.0.0.1:8081:0" -- Replace with the actual NodeId
      let nodeId2 = NodeId . EndPointAddress $ "127.0.0.1:8082:0" -- Replace with the actual NodeId
      let peers = [nodeId1, nodeId2] -- You can add more peers here
      runProcess localNode $ master peers

-- >>

module DistributedP
  ( main,
  )
where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Data.Binary
import Data.Typeable
import GHC.Generics
import Network.Transport.TCP

main :: IO ()
main = do
  Right transport <- createTransport (defaultTCPAddr "127.0.0.1" "8080") defaultTCPParameters
  localNode <- newLocalNode transport initRemoteTable
  runProcess localNode $ do
    master2

{-
self <- getSelfPid
send self ("Hello, Distributed World!" :: String)
message <- expect :: Process String
liftIO $ putStrLn message
-}

data Protocol
  = Init {masterPid :: ProcessId, pongPid :: ProcessId}
  | Ping ProcessId
  | Ping2
  | Pong
  | Done
  deriving (Typeable, Generic, Show)

instance Binary Protocol

ping :: Process ()
ping = do
  Init mPid pongPid <- expect
  myPid <- getSelfPid
  send pongPid (Ping myPid)
  Pong <- expect
  send mPid Done

pong :: Process ()
pong = do
  Ping pingPid <- expect
  send pingPid Pong

master :: Process ()
master = do
  myPid <- getSelfPid
  pingPid <- spawnLocal ping
  pongPid <- spawnLocal pong
  _ <- send pingPid (Init myPid pongPid)
  Done <- expect
  liftIO $ putStrLn "done"
  return ()

master2 :: Process ()
master2 = do
  (xs, xr) <- newChan
  spawnLocal (ping2 (xs, xr))
  spawnLocal (pong2 (xs, xr))
  ss <- receiveChan xr
  case ss of
    Done -> liftIO $ putStrLn "done2"
    xs -> liftIO $ do
      putStrLn "show xs"
      putStrLn (show xs)
  return ()

ping2 :: (SendPort Protocol, ReceivePort Protocol) -> Process ()
ping2 (xs, xr) = do
  sendChan xs Ping2
  liftIO $ putStrLn "send ping2"
  Pong <- receiveChan xr
  liftIO $ putStrLn "receive pong"
  liftIO $ putStrLn "send done"
  sendChan xs Done

pong2 :: (SendPort Protocol, ReceivePort Protocol) -> Process ()
pong2 (xs, xr) = do
  Ping2 <- receiveChan xr
  liftIO $ putStrLn "receive ping2"
  sendChan xs Pong
  liftIO $ putStrLn "send pong"

module DistributedP
  ( mainer,
  )
where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP

mainer :: IO ()
mainer = do
  Right transport <- createTransport "127.0.0.1" "8080" defaultTCPParameters
  localNode <- newLocalNode transport initRemoteTable
  runProcess localNode $ do
    self <- getSelfPid
    send self "Hello, Distributed World!"
    message <- expect :: Process String
    liftIO $ putStrLn message

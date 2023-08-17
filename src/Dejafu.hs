module Dejafu
  ( myFunction,
  )
where

import qualified Control.Concurrent.Classy as C

myFunction :: C.MonadConc m => m String
myFunction = do
  var <- C.newEmptyMVar
  C.fork (C.putMVar var "hello")
  C.fork (C.putMVar var "world")
  C.readMVar var

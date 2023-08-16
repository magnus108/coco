module Dispenser
  ( mainer,
    reset,
    unsafeTakeTicket,
    safeTakeTicket,
    start,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar (modifyMVar)

mainer :: IO ()
mainer = do
  val <- newEmptyMVar
  {-
  forkIO $ void $ reset val
  forkIO $ void $ unsafeTakeTicket val
  forkIO $ void $ unsafeTakeTicket val
  forkIO $ void $ unsafeTakeTicket val
  forkIO $ void $ reset val
  forkIO $ void $ unsafeTakeTicket val
  -}

  result <-
    mapConcurrently
      ( \x -> case x of
          'R' -> reset val
          'T' -> safeTakeTicket val
          'I' -> start val
      )
      ['I', 'T', 'T', 'R', 'T', 'T', 'T', 'T', 'T', 'T', 'T', 'T']
  putStrLn $ show $ (fmap show result)
  return ()

reset :: MVar Int -> IO Int
reset mv = do
  modifyMVar mv $ \x -> do
    return (0, 0)

start :: MVar Int -> IO Int
start mv = do
  putMVar mv 0
  return 0

safeTakeTicket :: MVar Int -> IO Int
safeTakeTicket mv = modifyMVar mv $ \currentVal -> do
  let newVal = currentVal + 1
  return (newVal, newVal)

unsafeTakeTicket :: MVar Int -> IO Int
unsafeTakeTicket mv = do
  currentVal <- readMVar mv
  let newVal = currentVal + 1
  _ <- takeMVar mv
  putMVar mv newVal
  return newVal

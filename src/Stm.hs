module Stm
  ( mainer,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async
import Control.Monad.STM (retry)

mainer2 :: IO ()
mainer2 = do
  resourceA <- atomically $ newTVar 1
  resourceB <- atomically $ newTVar 1

  thread1 <- async $ atomically $ do
    a <- readTVar resourceA
    b <- readTVar resourceB
    writeTVar resourceA (a + 1)
    writeTVar resourceB (b + 1)
    return ()

  thread2 <- async $ atomically $ do
    b <- readTVar resourceB
    a <- readTVar resourceA
    writeTVar resourceB (b + 1)
    writeTVar resourceA (a + 1)
    return ()

  wait thread1
  wait thread2

  a <- atomically $ readTVar resourceA
  b <- atomically $ readTVar resourceB
  putStrLn $ show a
  putStrLn $ show b

mainer :: IO ()
mainer = do
  let n = 100

  result <- atomically $ newTVar (0, 0)

  mapM_ (\x -> forkIO $ atomically $ addToResult result x) [1 .. n]

  sum <- atomically $ waitForCounter result n

  putStrLn $ "Sum [1..n] = " ++ show sum

type Result = TVar (Int, Int)

addToResult :: Result -> Int -> STM ()
addToResult result x = do
  (sum, endCtr) <- readTVar result
  writeTVar result (sum + x, endCtr + 1)

waitForCounter :: Result -> Int -> STM Int
waitForCounter result limit = do
  (sum, endCtr) <- readTVar result
  if endCtr < limit then retry else return sum

mainerBad :: IO ()
mainerBad = do
  resourceA <- newMVar True
  resourceB <- newMVar True

  thread1 <- async $ do
    a <- takeMVar resourceA
    b <- takeMVar resourceB
    return ()

  thread2 <- async $ do
    b <- takeMVar resourceB
    a <- takeMVar resourceA
    return ()

  wait thread1
  wait thread2

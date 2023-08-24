module PrintInOrder
  ( mainer,
  )
where

import qualified Control.Concurrent.Classy as C
import qualified Control.Concurrent.Classy.Async as A
import Control.Concurrent.Classy.MVar as MVar
import qualified Control.Concurrent.Classy.STM as STM

mainer :: (MonadIO m, C.MonadConc m) => m ()
mainer = do
  c <- MVar.newMVar []
  lock0 <- MVar.newMVar 0
  lock1 <- MVar.newEmptyMVar
  lock2 <- MVar.newEmptyMVar
  lock3 <- MVar.newEmptyMVar
  lock4 <- MVar.newEmptyMVar
  lock5 <- MVar.newEmptyMVar
  A.mapConcurrently_ (logIt c) [(lock0, "a", lock1), (lock1, "b", lock2), (lock2, "c", lock3), (lock3, "d", lock4), (lock4, "e", lock5)]
  xs <- MVar.readMVar c
  putStrLn (show xs)

logIt :: C.MonadConc m => MVar.MVar m [String] -> (MVar.MVar m Int, String, MVar.MVar m Int) -> m ()
logIt varA (var1, x, var2) = do
  _ <- MVar.takeMVar var1
  MVar.modifyMVar_ varA (\xs -> return $ x : xs)
  MVar.putMVar var2 0
  return ()

{-
mainer :: (MonadIO m, C.MonadConc m) => m ()
mainer = do
  c <- C.atomically $ TVar.newTVar (0, [])
  A.mapConcurrently_ (C.atomically . logIt c) [("a", 0), ("b", 1), ("c", 2), ("d", 3), ("e", 4)]
  xs <- C.atomically $ TVar.readTVar c
  putStrLn (show xs)

logIt :: (C.MonadSTM m) => TVar.TVar m (Int, [String]) -> (String, Int) -> m ()
logIt c (x, i) = do
  (j, xs) <- TVar.readTVar c
  STM.check (j == i)
  TVar.writeTVar c (j + 1, x : xs)
-}
{-
mainer :: (MonadIO m, MonadFail m, C.MonadConc m) => m ()
mainer = do
  hSetBuffering stdout NoBuffering
  var1 <- T.newEmptyMVar
  var2 <- T.newEmptyMVar
  f <- A.async (first' var1)
  s <- A.async (second' var1 var2)
  t <- A.async (third' var2)
  A.wait f
  A.wait s
  A.wait t

first' :: (MonadIO m, C.MonadConc m) => T.MVar m Int -> m ()
first' var1 = do
  putStrLn "first"
  T.putMVar var1 0

second' :: (MonadIO m, C.MonadConc m) => T.MVar m Int -> T.MVar m Int -> m ()
second' var1 var2 = do
  T.takeMVar var1
  putStrLn "second"
  T.putMVar var2 0

third' :: (MonadIO m, C.MonadConc m) => T.MVar m Int -> m ()
third' var2 = do
  T.takeMVar var2
  putStrLn "third"
  -}

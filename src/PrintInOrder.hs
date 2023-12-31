module PrintInOrder
  ( mainer,
  )
where

import qualified Control.Concurrent.Classy as C
import qualified Control.Concurrent.Classy.Async as A
import Control.Concurrent.Classy.MVar as MVar
import qualified Control.Concurrent.Classy.STM as STM
import qualified Control.Concurrent.Classy.STM.TMVar as TMVar
import qualified Control.Concurrent.Classy.STM.TVar as TVar

mainer :: (MonadIO m, C.MonadConc m) => m ()
mainer = do
  lock0 <- C.atomically $ TMVar.newTMVar []
  lock1 <- C.atomically $ TMVar.newEmptyTMVar
  lock2 <- C.atomically $ TMVar.newEmptyTMVar
  lock3 <- C.atomically $ TMVar.newEmptyTMVar
  lock4 <- C.atomically $ TMVar.newEmptyTMVar
  lock5 <- C.atomically $ TMVar.newEmptyTMVar
  lock6 <- C.atomically $ TMVar.newEmptyTMVar
  lock7 <- C.atomically $ TMVar.newEmptyTMVar
  lock8 <- C.atomically $ TMVar.newEmptyTMVar
  lock9 <- C.atomically $ TMVar.newEmptyTMVar
  lock10 <- C.atomically $ TMVar.newEmptyTMVar
  lock11 <- C.atomically $ TMVar.newEmptyTMVar
  lock12 <- C.atomically $ TMVar.newEmptyTMVar
  lock13 <- C.atomically $ TMVar.newEmptyTMVar
  lock14 <- C.atomically $ TMVar.newEmptyTMVar
  lock15 <- C.atomically $ TMVar.newEmptyTMVar
  A.mapConcurrently_
    (C.atomically . logIt)
    [ (lock0, "a", lock1),
      (lock1, "b", lock2),
      (lock2, "c", lock3),
      (lock3, "d", lock4),
      (lock4, "e", lock5),
      (lock5, "f", lock6),
      (lock6, "g", lock7),
      (lock7, "h", lock8),
      (lock8, "j", lock9),
      (lock9, "k", lock10),
      (lock10, "l", lock11),
      (lock11, "m", lock12),
      (lock12, "n", lock13),
      (lock13, "o", lock14),
      (lock14, "p", lock15)
    ]
  xs <- C.atomically $ TMVar.readTMVar lock15
  putStrLn (show xs)

logIt :: C.MonadSTM m => (TMVar.TMVar m [String], String, TMVar.TMVar m [String]) -> m ()
logIt (var1, x, var2) = do
  xs <- TMVar.takeTMVar var1
  let e = x : xs
  TMVar.putTMVar var2 e

{-
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
  xs <- MVar.takeMVar varA
  let e = x : xs
  MVar.putMVar varA e
  MVar.putMVar var2 0
  seq e (return ())
  return ()
-}
{-
mainer :: (MonadIO m, C.MonadConc m) => m ()
mainer = do
  c <- C.atomically $ TVar.newTVar (0, [])
  A.mapConcurrently_ (C.atomically . logIt c) [("a", 0), ("b", 1), ("c", 2), ("d", 3)]
  xs <- C.atomically $ TVar.readTVar c
  putStrLn (show xs)

logIt :: (C.MonadSTM m) => TVar.TVar m (Int, [String]) -> (String, Int) -> m ()
logIt c (x, i) = do
  (j, xs) <- TVar.readTVar c
  STM.check (j == i)
  let a = j + 1
  let b = x : xs
  TVar.writeTVar c (a, b)
  seq a (return ())
  seq b (return ())
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

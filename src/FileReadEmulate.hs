{-# LANGUAGE BangPatterns #-}

module FileReadEmulate
  ( findIt,
    mainer,
    findIt2,
  )
where

import qualified Control.Concurrent.Classy as C
import qualified Control.Concurrent.Classy.Async as A
import qualified Control.Concurrent.Classy.IORef as IORef
import Control.Monad.Catch (finally)

mainer :: (C.MonadConc m, MonadIO m) => m ()
mainer = do
  sem <- newNBSem 4
  r <- findIt3 sem 300000 [0 .. 600000]
  print r

findIt :: (C.MonadConc m) => Int -> [Int] -> m (Maybe Int)
findIt s dx = do
  let yx = sort dx
  case yx of
    [] -> return Nothing
    (x : xs) -> if s == x then return (Just x) else findIt s xs

findIt2 :: (C.MonadConc m) => Int -> [Int] -> m (Maybe Int)
findIt2 s dx = do
  let yx = sort dx
  case yx of
    [] -> return Nothing
    (x : xs) -> if s == x then return (Just x) else foldr (\z f -> subFindIt s z f) dowait xs []
  where
    dowait as = loop (reverse as)
    loop [] = return Nothing
    loop (a : as) = do
      r <- A.wait a
      case r of
        Nothing -> loop as
        Just a -> return (Just a)

subFindIt ::
  C.MonadConc m =>
  Int ->
  Int ->
  ([A.Async m (Maybe Int)] -> m b) ->
  [A.Async m (Maybe Int)] ->
  m b
subFindIt s p inner asyncs = A.withAsync (findIt2 s [p]) $ \a -> inner (a : asyncs)

findIt3 :: (C.MonadConc m) => NBSem m -> Int -> [Int] -> m (Maybe Int)
findIt3 sem s dx = do
  let yx = sort dx
  case yx of
    [] -> return Nothing
    (x : xs) -> if s == x then return (Just x) else foldr (\z f -> subFindIt3 sem s xs f) dowait xs []
  where
    dowait as = loop (reverse as)
    loop [] = return Nothing
    loop (a : as) = do
      r <- A.wait a
      case r of
        Nothing -> loop as
        Just a -> return (Just a)

subFindIt3 ::
  C.MonadConc m =>
  NBSem m ->
  Int ->
  [Int] ->
  ([A.Async m (Maybe Int)] -> m (Maybe Int)) ->
  [A.Async m (Maybe Int)] ->
  m (Maybe Int)
subFindIt3 sem s p inner asyncs = do
  q <- tryWaitNBSem sem
  if q
    then do
      A.withAsync (findIt3 sem s p `finally` signalNBSem sem) $ \a -> inner (a : asyncs)
    else do
      r <- findIt3 sem s p
      case r of
        Nothing -> inner asyncs
        Just _ -> return r

newtype NBSem m = NBSem (C.IORef m Int)

newNBSem :: C.MonadConc m => Int -> m (NBSem m)
newNBSem i = do
  m <- IORef.newIORef i
  return (NBSem m)

tryWaitNBSem :: C.MonadConc m => NBSem m -> m Bool
tryWaitNBSem (NBSem m) = do
  IORef.atomicModifyIORef m $ \i ->
    if i == 0
      then (i, False)
      else let !z = i - 1 in (z, True)

signalNBSem :: C.MonadConc m => NBSem m -> m ()
signalNBSem (NBSem m) =
  IORef.atomicModifyIORef m $ \i ->
    let !z = i + 1 in (z, ())

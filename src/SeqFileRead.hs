{-# LANGUAGE BangPatterns #-}

module SeqFileRead
  ( find_,
    find_2,
    find_3,
    mainer,
    mainer2,
    mainer3,
  )
where

import qualified Control.Concurrent.Classy as C
import qualified Control.Concurrent.Classy.Async as A
import qualified Control.Concurrent.Classy.IORef as IORef
import qualified Control.Concurrent.Classy.Lock as L
import Control.Monad.Catch (finally)
import System.FilePath
import UnliftIO.Directory

mainer :: (C.MonadConc m, MonadIO m) => m ()
mainer = do
  r <- find_ "SeqFileRead.hs" "/Users/magnus/Desktop"
  print r

mainer2 :: (C.MonadConc m, MonadIO m) => m ()
mainer2 = do
  r <- find_2 "SeqFileRead.hs" "/Users/magnus/Desktop/haskell"
  print r

mainer3 :: (C.MonadConc m, MonadIO m) => m ()
mainer3 = do
  sem <- newNBSem 4
  r <- find_3 sem "SeqFileRead.hs" "/Users/magnus/Desktop/haskell"
  print r

find_ :: (C.MonadConc m, MonadIO m) => String -> FilePath -> m (Maybe FilePath)
find_ s d = do
  fs <- getDirectoryContents d
  let fs' = sort $ filter (`notElem` [".", ".."]) fs
  if s `elem` fs'
    then return (Just (d </> s))
    else loop fs'
  where
    loop [] = return Nothing
    loop (f : fs) = do
      let d' = d </> f
      isdir <- doesDirectoryExist d'
      if isdir
        then do
          r <- find_ s d'
          case r of
            Just _ -> return r
            Nothing -> loop fs
        else loop fs

find_2 :: (MonadIO m, C.MonadConc m) => String -> FilePath -> m (Maybe FilePath)
find_2 s d = do
  fs <- getDirectoryContents d
  let fs' = sort $ filter (`notElem` [".", ".."]) fs
  if s `elem` fs'
    then return (Just (d </> s))
    else do
      let ps = map (d </>) fs'
      foldr (subfind s) dowait ps []
  where
    dowait as = loop (reverse as)
    loop [] = return Nothing
    loop (a : as) = do
      r <- A.wait a
      case r of
        Nothing -> loop as
        Just a -> return (Just a)

subfind ::
  (MonadIO m, C.MonadConc m) =>
  String ->
  FilePath ->
  ([A.Async m (Maybe FilePath)] -> m (Maybe FilePath)) ->
  [A.Async m (Maybe FilePath)] ->
  m (Maybe FilePath)
subfind s p inner asyncs = do
  isdir <- doesDirectoryExist p
  if not isdir
    then inner asyncs
    else A.withAsync (find_2 s p) $ \a -> inner (a : asyncs)

find_3 :: (MonadIO m, C.MonadConc m) => NBSem m -> String -> FilePath -> m (Maybe FilePath)
find_3 lock s d = do
  fs <- getDirectoryContents d
  let fs' = sort $ filter (`notElem` [".", ".."]) fs
  if s `elem` fs'
    then return (Just (d </> s))
    else do
      let ps = map (d </>) fs'
      foldr (subfind2 lock s) dowait ps []
  where
    dowait as = loop (reverse as)
    loop [] = return Nothing
    loop (a : as) = do
      r <- A.wait a
      case r of
        Nothing -> loop as
        Just a -> return (Just a)

subfind2 ::
  (MonadIO m, C.MonadConc m) =>
  NBSem m ->
  String ->
  FilePath ->
  ([A.Async m (Maybe FilePath)] -> m (Maybe FilePath)) ->
  [A.Async m (Maybe FilePath)] ->
  m (Maybe FilePath)
subfind2 lock s p inner asyncs = do
  isdir <- doesDirectoryExist p
  if not isdir
    then inner asyncs
    else do
      q <- tryWaitNBSem lock
      if q
        then do
          let dofind = find_3 lock s p `finally` signalNBSem lock
          A.withAsync dofind $ \a -> inner (a : asyncs)
        else do
          r <- find_3 lock s p
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

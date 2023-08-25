module SeqFileRead
  ( find_,
    find_2,
  )
where

import qualified Control.Concurrent.Classy as C
import qualified Control.Concurrent.Classy.Async as A
import System.FilePath
import UnliftIO.Directory

find_ :: String -> FilePath -> IO (Maybe FilePath)
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

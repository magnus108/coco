module Deadlock
  ( mainer,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Relude.Unsafe ((!!))

mainer :: IO ()
mainer = do
  let n = 3
  mvars <- mapM (\x -> newMVar x) [0 .. n]
  result <-
    mapConcurrently
      ( \x -> do
          let m1 = mvars !! (x `mod` n)
          let m2 = mvars !! ((x + 1) `mod` n)
          val1 <- takeMVar m1
          val2 <- takeMVar m2
          putMVar m2 (val1)
          putMVar m1 (val2)
      )
      [0, 1, 1, 1, 1, 1, 1]
  putStrLn $ show $ (fmap show result)
  return ()

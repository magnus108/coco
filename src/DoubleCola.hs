module DoubleCola
  ( mainer,
  )
where

import Control.Comonad
import Control.Comonad.Representable.Store
import Data.Foldable
import Data.List (intercalate, last)
import Data.Stream.Infinite
import Prelude hiding (last)

doubleCola :: Store Stream Int
doubleCola = extend wfix $ store go 0
  where
    go 0 w = 0
    go 1 w = 1
    go 2 w = 2
    go 3 w = 3
    go 4 w = 4
    go n w = last $ experiment (\x -> [x - 5, x - 4, x - 3, x - 2, x - 1]) w

mainer :: IO ()
mainer = putStrLn $ show $ peek 5 doubleCola

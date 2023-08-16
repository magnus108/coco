module DoubleCola
  ( mainer,
  )
where

import Control.Comonad
import Control.Comonad.Representable.Store
import Data.Foldable
import Data.List (intercalate)
import Data.Stream.Infinite

doubleCola :: Store Stream Int
doubleCola = undefined {-extend wfix $ store go 0
                       where
                         go 0 w = 0
                         go 1 w = 1
                         go 2 w = 2
                         go 3 w = 3
                         go 4 w = 4
                         go n w = peeks (n - 1) $ seek 1 w
                         -}

mainer :: IO ()
mainer = do
  putStrLn $ show $ peek 2 doubleCola
  putStrLn $ show $ peek 5 doubleCola
  putStrLn $ show $ peek 6 doubleCola

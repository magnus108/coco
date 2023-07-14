module Fibo (factorialStreamW, llol) where

import Control.Comonad
import Control.Comonad.Representable.Store
import Data.Foldable
import Data.List (intercalate)
import Data.Stream.Infinite

factorialStreamW :: Store Stream Int
factorialStreamW = extend wfix $ store go 0
  where
    go 0 w = 0
    go 1 w = 1
    go a w =
      let prev2 = peek (a - 2) w
          prev1 = peek (a - 1) w
       in traceShow ("1-" ++ show prev1) prev1 + (traceShow ("2-" ++ show prev2) prev2)

factorialStreamF :: Store Stream Int
factorialStreamF = fix $ \result -> store (flip go result) 0
  where
    go 0 w = 0
    go 1 w = 1
    go a w =
      let prev2 = peek (a - 2) w
          prev1 = peek (a - 1) w
       in traceShow ("1-" ++ show prev1) prev1 + (traceShow ("2-" ++ show prev2) prev2)

llol = peek 10 factorialStreamF

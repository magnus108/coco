module Piece
  ( main,
  )
where

import qualified Server
import qualified SpecIt
import qualified SpecIt2

main :: IO ()
main = do
  SpecIt2.main

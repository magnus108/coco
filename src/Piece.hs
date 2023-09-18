module Piece
  ( main,
  )
where

import qualified Server
import qualified SpecIt
import qualified SpecIt2

main :: IO ()
main = do
  Server.main

module Piece
  ( main,
  )
where

import qualified Account
import qualified Db
import qualified Server
import qualified SpecIt
import qualified SpecIt2

main :: IO ()
main = do
  Db.main

module Piece
  ( main,
    entersGroundLevel,
    parse,
  )
where

import Control.Comonad
import qualified ListZipper as LZ

-- Counting Valleys problem
countValleys :: String -> Int
countValleys = maybe 0 solve . parse

solve :: LZ.ListZipper Int -> Int
solve lz = lz =>> entersGroundLevel & LZ.toList & fmap fromEnum & sum

parse :: [Char] -> Maybe (LZ.ListZipper Int)
parse = foldlM (\previous current -> LZ.forward $ LZ.insert ((if up current then (+) else (-)) (extract previous) 1) previous) (LZ.singleton 0) <=< LZ.fromList

up :: Char -> Bool
up = (==) 'U'

entersGroundLevel :: LZ.ListZipper Int -> Bool
entersGroundLevel hill = extract hill == 0 && maybe False (\x -> extract x == -1) (LZ.backward hill)

main :: IO ()
main = do
  let path = "UDDDUDUUUDDU"
  let valleyCount = countValleys path
  print valleyCount

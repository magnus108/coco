module Piece
  ( main,
    entersGroundLevel,
    parse,
    move,
    mountainPeeks,
    isSequence,
  )
where

import Control.Comonad
import Control.Comonad.Store
import qualified Data.Set as S
import qualified ListZipper as LZ

-- Counting Valleys problem
countValleys :: String -> Int
countValleys = maybe 0 solve . parse

solve :: LZ.ListZipper Int -> Int
solve lz = lz =>> entersGroundLevel & LZ.toList & fmap fromEnum & sum

parse :: String -> Maybe (LZ.ListZipper Int)
parse = LZ.fromList >=> heights

heights :: LZ.ListZipper Char -> Maybe (LZ.ListZipper Int)
heights = foldlM (\steps movement -> LZ.forward (LZ.insert (nextStep steps movement) steps)) (LZ.singleton 0)

nextStep :: LZ.ListZipper Int -> Char -> Int
nextStep steps movement = (if up movement then (+) else (-)) (extract steps) 1

up :: Char -> Bool
up = (==) 'U'

entersGroundLevel :: LZ.ListZipper Int -> Bool
entersGroundLevel hill = extract hill == 0 && maybe False (\x -> extract x == -1) (LZ.backward hill)

-- Counting mountains problem
mountainPeeks :: LZ.ListZipper Int -> Bool
mountainPeeks hill = maybe False (\prev -> extract prev < extract hill) (LZ.backward hill) && maybe False (\next -> extract next < extract hill) (LZ.forward hill)

countMountains :: String -> Int
countMountains = maybe 0 solveMountains . parse

solveMountains :: LZ.ListZipper Int -> Int
solveMountains lz = lz =>> mountainPeeks & LZ.toList & fmap fromEnum & sum

-- Step Sequenceing problem

neighbourLocations :: (Sum Int, Sum Int) -> [(Sum Int, Sum Int)]
neighbourLocations location = mappend location <$> [(-1, 0), (0, -1), (0, 1), (1, 0)]

move :: [(Sum Int, Sum Int)] -> [(Sum Int, Sum Int)]
move movementss = traceShowId list
  where
    list = foldl' (\(y : acc) x -> (mappend y x) : y : acc) [(0, 0)] movementss

isSequence :: [(Sum Int, Sum Int)] -> Bool
isSequence xs = and $ experiment (\s -> xs) $ extend (even . sum . extract) $ extend (experiment neighbourLocations) $ store (\y -> (*) 2 $ length $ filter (\x -> x == y) xs) (0, 0)

main :: IO ()
main = do
  let path = "UDUDDDDDUD" -- 01010
  let valleyCount = countValleys path
  let mountainCount = countMountains path
  let pathSequence = "URDL" -- LOOP
  print "nono"

--  print valleyCount
-- print mountainCount
-- print pathSequence

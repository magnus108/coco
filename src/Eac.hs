module Eac
  ( generations,
    rule,
    begin,
    nextGeneration,
    toStates,
  )
where

import Control.Comonad
import Control.Comonad.Store
import qualified Data.Map.Strict as M
import Relude.List

type TransRule = M.Map [Bool] Bool

type Row = Store Int Bool

inputCoordinate :: Int -> [Int]
inputCoordinate i = [i - 1, i, i + 1]

input :: Row -> [Bool]
input = experiment inputCoordinate

bin :: Int -> Int -> [Bool]
bin upper = reverse . go (0 :: Int)
  where
    go i m
      | i < upper = odd m : go (i + 1) (m `quot` 2)
      | otherwise = []

fromRule :: Int -> [Bool]
fromRule = bin 8

fromConfiguration :: Int -> [Bool]
fromConfiguration = bin 3

rule :: Int -> TransRule
rule =
  fromList
    . zipWith (\i s -> (fromConfiguration i, s)) [7, 6 .. 0]
    . fromRule

nextCell :: TransRule -> Row -> Bool
nextCell tr row = tr M.! input row

nextGeneration :: TransRule -> Row -> Row
nextGeneration = extend . nextCell

generations :: TransRule -> Row -> [Row]
generations = iterate . nextGeneration

begin :: [Bool] -> Row
begin v = store (\i -> fromMaybe False $ (!!?) v i) 0

toStates :: Row -> [Bool]
toStates = experiment (const [0 .. 9])

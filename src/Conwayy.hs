{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Conwayy (startingGrid, animateGrid) where

import Control.Applicative (ZipList (..))
import Control.Comonad
import Control.Comonad.Store
import Control.Parallel.Strategies
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Monoid (Ap (..), Sum (..))
import qualified Data.Set as S
import Data.String (lines, unlines)
import Prelude hiding (lines, unlines)

-- | We represent a coordinate as a product of sums so we can use 'mappend' for easy shifting
type Coord = (Sum Int, Sum Int)

-- | A game of life grid is a store from Coords to Booleans
type Grid = Store Coord Bool

-- | A type containing a slot for each neighbour
data Neighbours a
  = Neighbours
      a
      a
      a
      a
      a
      a
      a
      a
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Given a coordinate, compute all the neighbours of that position.
neighbourLocations :: Coord -> Neighbours Coord
neighbourLocations s =
  mappend s
    <$> Neighbours
      (-1, -1)
      (0, -1)
      (1, -1)
      (-1, 0)
      (1, 0)
      (-1, 1)
      (0, 1)
      (1, 1)

-- | A cell is alive in the next iteration IFF:
-- * The cell is currently ALIVE AND it has 2 living neighbours
-- OR
-- * The cell has exactly 3 living neighbours
-- OTHERWISE the cell is dead in the next iteration
checkCellAlive :: Grid -> Bool
checkCellAlive grid =
  case (currentCellAlive, numLivingNeighbours) of
    (True, 2) -> True
    (_, 3) -> True
    _ -> False
  where
    currentCellAlive :: Bool
    currentCellAlive = extract grid
    neighboursAlive :: Neighbours Bool
    neighboursAlive = experiment neighbourLocations grid
    numLivingNeighbours :: Int
    numLivingNeighbours = length . filter id . toList $ neighboursAlive

-- | Iterate the game of life by one step
step :: Grid -> Grid
step = extend checkCellAlive

-- | The starting state of the grid
startingGrid :: Grid
startingGrid = store checkAlive (0, 0)
  where
    checkAlive :: Coord -> Bool
    checkAlive coord = S.member coord livingCells

    livingCells :: S.Set Coord
    livingCells = S.fromList [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]

---- HELPERS

drawGrid :: Int -> Grid -> String
drawGrid size g = unlines $ parMap rdeepseq (\x -> fmap (\y -> toChar $ peek (Sum x, Sum y) g) [0 .. size - 1]) [0 .. size - 1]
  where
    toChar True = '#'
    toChar False = '.'

-- | print out a 10x10 chunk of a grid
printGrid :: Grid -> IO ()
printGrid = putStrLn . drawGrid 10

-- | Show several steps of the game side by side
animateGrid :: Grid -> IO ()
animateGrid grid =
  -- Don't worry about this implementation
  -- it gets super slow at larger iterations, for good reason!
  -- It recomputes every step in all previous iterations for each next iteration
  -- You can fix this using Comonad.Representable.Store :)
  putStrLn
    . unlines
    . getZipList
    . getAp
    . foldMap Ap
    . intersperse (pure "|")
    . fmap (ZipList . lines . drawGrid 30)
    . take 8
    $ iterate step grid

-- | A helper for transposing shapes onto positions in the grid
at :: [Coord] -> Coord -> [Coord]
coords `at` origin = map (<> origin) coords

-- | Several well known Conway's game of life shapes
glider, blinker, beacon :: [Coord]
glider = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
blinker = [(0, 0), (1, 0), (2, 0)]
beacon = [(0, 0), (1, 0), (0, 1), (3, 2), (2, 3), (3, 3)]

conwayGrid :: Store (Sum Int, Sum Int) Bool
conwayGrid = store checkAlive (0, 0)
  where
    checkAlive :: Coord -> Bool
    checkAlive coord = S.member coord livingCells
    livingCells :: S.Set Coord
    livingCells = S.fromList glider

module WordSearch2
  ( animateGrid,
    startingGrid,
  )
where

import Control.Comonad
import Control.Comonad.Store
import qualified Data.Map.Strict as M
import Data.String (lines, unlines)
import Prelude hiding (lines, unlines)

type Coord = (Sum Int, Sum Int)

type Grid = Store Coord [String]

data Neighbours a = Neighbours a a a a
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Given a coordinate, compute all the neighbours of that position.
neighbourLocations :: Coord -> Neighbours Coord
neighbourLocations s = mappend s <$> Neighbours (0, -1) (-1, 0) (1, 0) (0, 1)

-- | CheckWord
checkWord :: Grid -> [String]
checkWord grid = joint
  where
    currentCell :: [String]
    currentCell = extract grid

    neighbours :: Neighbours [String]
    neighbours = experiment neighbourLocations grid

    joint :: [String]
    joint = do
      x <- currentCell
      ys <- toList neighbours
      filter (flip isPrefixOf "aaa") $ fmap (++ x) ys

step :: Grid -> Grid
step = extend checkWord

startingGrid :: Grid
startingGrid = store lookup (0, 0)
  where
    lookup :: Coord -> [String]
    lookup coord = M.findWithDefault [] coord cells

    cells :: M.Map Coord [String]
    cells =
      M.fromList
        [ ((0, 3), ["o"]),
          ((1, 3), ["a"]),
          ((2, 3), ["a"]),
          ((3, 3), ["n"]),
          ((0, 2), ["e"]),
          ((1, 2), ["t"]),
          ((2, 2), ["a"]),
          ((3, 2), ["e"]),
          ((0, 1), ["i"]),
          ((1, 1), ["h"]),
          ((2, 1), ["k"]),
          ((3, 1), ["r"]),
          ((0, 0), ["i"]),
          ((1, 0), ["f"]),
          ((2, 0), ["l"]),
          ((3, 0), ["v"])
        ]

drawGrid :: Int -> Grid -> String
drawGrid size g = unlines $ do
  x <- [0 .. size - 1]
  return $ do
    y <- [0 .. size - 1]
    toChar $ peek (Sum x, Sum y) g
  where
    toChar [] = "X"
    toChar xs = concat xs

printGrid :: Grid -> IO ()
printGrid = putStrLn . drawGrid 4

animateGrid :: Grid -> IO ()
animateGrid grid =
  putStrLn
    . unlines
    . getZipList
    . getAp
    . foldMap Ap
    . intersperse (pure "|")
    . fmap (ZipList . lines . drawGrid 4)
    . take 3
    $ iterate step grid

-- find one word first
-- Input: board = [["o","a","a","n"],["e","t","a","e"],["i","h","k","r"],["i","f","l","v"]], words = ["oath","pea","eat","rain"]
-- Output: ["eat","oath"]

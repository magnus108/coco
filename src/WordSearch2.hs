module WordSearch2
  ( animateGrid,
    startingGrid,
    startingGrid2,
    printGrid,
    resGrid,
    resGrid2,
    nextGrid,
    nextNextGrid,
    nextNextNextGrid,
    nextNextNextNextGrid,
    res,
    res2,
    move,
    Coord,
    Grid,
  )
where

import Control.Comonad
import Control.Comonad.Env
import Control.Comonad.Store
import Data.List hiding (elem)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.String (lines, unlines)
import Prelude hiding (ask, lines, tail, unlines)

type Coord = (Sum Int, Sum Int)

type Grid = EnvT String (Store Coord) (S.Set String)

data Neighbours a = Neighbours a a a a
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Given a coordinate, compute all the neighbours of that position.
neighbourLocations :: Coord -> Neighbours Coord
neighbourLocations s = mappend s <$> Neighbours (0, -1) (-1, 0) (1, 0) (0, 1)

-- | CheckWord
checkWord :: Grid -> S.Set String
checkWord grid = S.fromList joint
  where
    currentCell :: S.Set String
    currentCell = extract grid

    neighbours :: Neighbours (S.Set String)
    neighbours = experiment neighbourLocations grid

    joint :: [String]
    joint = do
      x <- S.toList currentCell
      xs <- toList neighbours
      (filter (flip isPrefixOf (ask grid)) $ fmap (\y -> y ++ x) (S.toList xs)) <|> [x] -- (filter (flip isSubsequenceOf (ask grid)) (S.toList xs))

step :: Grid -> Grid
step = extend checkWord

nextGrid :: Grid
nextGrid = step startingGrid

nextNextGrid :: Grid
nextNextGrid = step nextGrid

nextNextNextGrid :: Grid
nextNextNextGrid = step nextNextGrid

nextNextNextNextGrid :: Grid
nextNextNextNextGrid = step nextNextNextGrid

startingGrid2 :: M.Map Coord (S.Set String) -> Coord -> Store Coord (S.Set String)
startingGrid2 cells pos = store lookup pos
  where
    lookup :: Coord -> S.Set String
    lookup coord = M.findWithDefault S.empty coord cells

startingGrid :: Grid
startingGrid = EnvT "eat" $ store lookup (0, 0)
  where
    lookup :: Coord -> S.Set String
    lookup coord = M.findWithDefault S.empty coord cells

    cells :: M.Map Coord (S.Set String)
    cells =
      M.fromList
        [ ((0, 3), S.singleton "o"),
          ((1, 3), S.singleton "a"),
          ((2, 3), S.singleton "a"),
          ((3, 3), S.singleton "n"),
          ((0, 2), S.singleton "e"),
          ((1, 2), S.singleton "t"),
          ((2, 2), S.singleton "a"),
          ((3, 2), S.singleton "e"),
          ((0, 1), S.singleton "i"),
          ((1, 1), S.singleton "h"),
          ((2, 1), S.singleton "k"),
          ((3, 1), S.singleton "r"),
          ((0, 0), S.singleton "i"),
          ((1, 0), S.singleton "f"),
          ((2, 0), S.singleton "l"),
          ((3, 0), S.singleton "v")
        ]

drawGrid :: Int -> Grid -> String
drawGrid size g = unlines $ reverse $ do
  y <- [0 .. size - 1]
  return $ do
    x <- [0 .. size - 1]
    toChar . S.toList $ peek (Sum x, Sum y) g
  where
    toChar :: [String] -> String
    toChar [] = "-X-"
    toChar xs = "-" ++ (concat $ intersperse "|" xs) ++ "-"

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
    $ [step grid]

resGrid :: Int -> Grid -> [String]
resGrid size g = do
  y <- [0 .. size - 1]
  x <- [0 .. size - 1]
  S.toList $ peek (Sum x, Sum y) g

res :: Grid -> Bool
res grid = elem (ask grid) $ foldMap (resGrid 4) $ takeWhile2 (on (/=) (resGrid 4)) $ iterate step grid

move :: [(Sum Int, Sum Int)] -> String -> Store Coord (S.Set String) -> String
move [] s w = (concat $ S.toList $ extract w) ++ s
move (x : xs) s w = if ((extract w) == S.empty) then s else move xs (concat (S.toList (extract w)) ++ s) (seeks (mappend x) w)

resGrid2 :: Int -> Int -> Grid -> [String]
resGrid2 x' y' g = do
  x <- [0 .. x' - 1]
  y <- [0 .. y' - 1]
  S.toList $ peek (Sum x, Sum y) g

res2 :: Int -> Int -> Grid -> Bool
res2 x y grid = elem (ask grid) $ foldMap (resGrid2 x y) $ takeWhile2 (on (/=) (resGrid2 x y)) $ iterate step grid

takeWhile2 :: (a -> a -> Bool) -> [a] -> [a]
takeWhile2 _ [] = []
takeWhile2 _ [x] = [x]
takeWhile2 pred (x : y : xs)
  | pred x y = x : takeWhile2 pred (y : xs)
  | otherwise = [x]

-- find one word first
-- Input: board = [["o","a","a","n"],["e","t","a","e"],["i","h","k","r"],["i","f","l","v"]], words = ["oath","pea","eat","rain"]
-- Output: ["eat","oath"]

module Snail
  ( mainer,
  )
where

import Control.Comonad
import qualified ListZipper as LZ

input :: Maybe (LZ.ListZipper (LZ.ListZipper Int))
input =
  LZ.fromList
    ( catMaybes
        [ LZ.fromList [1, 4, 7, 7],
          LZ.fromList [2, 5, 8, 8],
          LZ.fromList [3, 6, 9, 9],
          LZ.fromList [1, 4, 7, 7]
        ]
    )

pop :: LZ.ListZipper a -> Maybe (LZ.ListZipper a)
pop (LZ.ListZipper [] x (r : rs)) = LZ.toLast $ LZ.ListZipper [] r rs
pop (LZ.ListZipper (l : ls) x []) = LZ.toFirst $ LZ.ListZipper ls l []
pop (LZ.ListZipper ls x rs) = Nothing

pop2 :: LZ.ListZipper a -> Maybe (LZ.ListZipper a)
pop2 (LZ.ListZipper [] x (r : rs)) = Just $ LZ.ListZipper [] r rs
pop2 (LZ.ListZipper (l : ls) x []) = Just $ LZ.ListZipper ls l []
pop2 (LZ.ListZipper ls x rs) = Nothing

moveRight :: LZ.ListZipper (LZ.ListZipper Int) -> Maybe (LZ.ListZipper (LZ.ListZipper Int))
moveRight lz@(LZ.ListZipper ls x rs) =
  case LZ.forward lz of
    Nothing -> (\x' -> LZ.ListZipper ls x' rs) <$> (LZ.toLast (extract lz))
    Just (LZ.ListZipper ls' x' rs') -> case pop (extract lz) of
      Nothing -> Nothing
      Just lz' -> Just $ LZ.ListZipper (lz' : ls) x' rs'

moveLeft :: LZ.ListZipper (LZ.ListZipper Int) -> Maybe (LZ.ListZipper (LZ.ListZipper Int))
moveLeft lz@(LZ.ListZipper ls x rs) =
  case LZ.backward lz of
    Nothing -> (\x' -> LZ.ListZipper ls x' rs) <$> (LZ.toFirst (extract lz))
    Just (LZ.ListZipper ls' x' rs') -> case pop (extract lz) of
      Nothing -> Nothing
      Just lz' -> Just $ LZ.ListZipper ls' x' (lz' : rs)

isFinished :: LZ.ListZipper (LZ.ListZipper Int) -> Bool
isFinished (LZ.ListZipper [] (LZ.ListZipper [] x []) []) = True
isFinished _ = False

mainer :: IO ()
mainer = do
  let x = isFinished <$> (moveLeft =<< pop2 =<< moveRight =<< moveRight =<< pop2 =<< moveLeft =<< moveLeft =<< moveLeft =<< pop2 =<< moveRight =<< moveRight =<< moveRight =<< moveRight =<< input)
  print x

-- popdown moveright

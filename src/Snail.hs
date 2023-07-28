module Snail
  ( mainer,
  )
where

import Control.Comonad
import Control.Monad.Loops
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
    Nothing -> Nothing
    Just (LZ.ListZipper ls' x' rs') -> case pop (extract lz) of
      Nothing -> Just $ LZ.ListZipper [] x' rs'
      Just lz' -> Just $ LZ.ListZipper (lz' : ls) x' rs'

moveLeft :: LZ.ListZipper (LZ.ListZipper Int) -> Maybe (LZ.ListZipper (LZ.ListZipper Int))
moveLeft lz@(LZ.ListZipper ls x rs) =
  case LZ.backward lz of
    Nothing -> Nothing
    Just (LZ.ListZipper ls' x' rs') -> case pop (extract lz) of
      Nothing -> Just $ LZ.ListZipper ls' x' []
      Just lz' -> Just $ LZ.ListZipper ls' x' (lz' : rs)

moveDown :: LZ.ListZipper (LZ.ListZipper Int) -> Maybe (LZ.ListZipper (LZ.ListZipper Int))
moveDown lz@(LZ.ListZipper ls x rs) =
  case LZ.forward (extract lz) of
    Nothing -> Nothing
    Just (LZ.ListZipper ls' x' rs') -> Just $ LZ.ListZipper ls (LZ.ListZipper [] x' rs') rs

moveUp :: LZ.ListZipper (LZ.ListZipper Int) -> Maybe (LZ.ListZipper (LZ.ListZipper Int))
moveUp lz@(LZ.ListZipper ls x rs) =
  case LZ.backward (extract lz) of
    Nothing -> Nothing
    Just (LZ.ListZipper ls' x' rs') -> Just $ LZ.ListZipper ls (LZ.ListZipper ls' x' []) rs

isFinished :: LZ.ListZipper (LZ.ListZipper Int) -> Bool
isFinished (LZ.ListZipper [] (LZ.ListZipper [] x []) []) = True
isFinished _ = False

repeatBindMaybe :: a -> (a -> Maybe a) -> Maybe a
repeatBindMaybe m f = case f m of
  Nothing -> Just m
  Just y -> repeatBindMaybe y f

repeatCyclicFuncs :: a -> [a -> Maybe a] -> a
repeatCyclicFuncs x funcs =
  case applyFuncs funcs (Just x) of
    Nothing -> x
    Just y -> repeatCyclicFuncs y funcs

applyFuncs :: [a -> Maybe a] -> Maybe a -> Maybe a
applyFuncs [] val = val
applyFuncs (f : fs) val = val >>= f >>= applyFuncs fs . Just

mainer :: IO ()
mainer = do
  let x =
        input
          >>= moveRight
          >>= moveRight
          >>= moveRight
          >>= moveDown
          >>= moveDown
          >>= moveDown
          >>= moveLeft
          >>= moveLeft
          >>= moveLeft
          >>= moveUp
          >>= moveUp
          >>= moveRight
          >>= moveRight
          >>= moveDown
          >>= moveLeft
          & maybe False isFinished
  print x
  --  let gg = iterateUntil isFinished $ fmap (flip repeatBindMaybe) $ cycle [moveRight, moveDown, moveLeft, moveUp]
  let gda = fmap (flip repeatBindMaybe) $ cycle [moveRight, moveDown, moveLeft, moveUp]
  let gg = input >>= \x -> Just $ repeatCyclicFuncs x gda
  print gg

--  let y = (repeatBindMaybe (repeatBindMaybe (repeatBindMaybe (repeatBindMaybe input moveRight) moveDown) moveLeft) moveUp)

-- popdown moveright

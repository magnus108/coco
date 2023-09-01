{-# LANGUAGE Rank2Types #-}

module Yoneda
  ( mainer,
  )
where

import Criterion.Main

newtype Yoneda f a = Yoneda {runYoneda :: forall b. (a -> b) -> f b}

instance Functor (Yoneda f) where
  fmap f m = Yoneda (\k -> runYoneda m (k . f))

instance Applicative f => Applicative (Yoneda f) where
  pure a = Yoneda (\f -> pure (f a))
  Yoneda m <*> Yoneda n = Yoneda (\f -> m (f .) <*> n id)

optimizeFmap :: (a -> b) -> [a] -> Yoneda [] b
optimizeFmap f xs = Yoneda (\g -> fmap (g . f) xs)

-- Your map functions
naiveMap :: [Int] -> [Int]
naiveMap xs = map (+ 1) $ map (+ 1) $ map (+ 1) $ map (+ 1) $ map (+ 1) $ map (+ 1) $ map (+ 1) $ map (+ 1) $ map (+ 1) $ map (+ 1) $ map (+ 1) $ map (+ 1) $ map (+ 1) $ map (+ 1) $ map (+ 1) $ map (+ 1) $ map (+ 1) $ map (+ 1) $ map (+ 1) $ map (+ 1) $ map (+ 1) (map (* 2) xs)

yonedaMap :: [Int] -> [Int]
yonedaMap xs = runYoneda (fmap (+ 1) $ fmap (+ 1) $ fmap (+ 1) $ fmap (+ 1) $ fmap (+ 1) $ fmap (+ 1) $ fmap (+ 1) $ fmap (+ 1) $ fmap (+ 1) $ fmap (+ 1) $ fmap (+ 1) $ fmap (+ 1) $ fmap (+ 1) $ fmap (+ 1) $ fmap (+ 1) $ fmap (+ 1) $ fmap (+ 1) $ fmap (+ 1) $ fmap (+ 1) $ fmap (+ 1) $ fmap (+ 1) (optimizeFmap (* 2) xs)) id

mainer :: IO ()
mainer =
  defaultMain
    [ bench "naive map" $ whnf naiveMap [1 .. 1000000],
      bench "yoneda map" $ whnf yonedaMap [1 .. 1000000]
    ]

{-# LANGUAGE Rank2Types #-}

module Yoneda () where

-- fmap :: (a -> b) -> f a -> f b
newtype Yoneda f a = Yoneda {runYoneda :: forall b. (a -> b) -> f b}

instance Functor (Yoneda f) where
  fmap f m = Yoneda (\k -> runYoneda m (k . f))

instance Applicative f => Applicative (Yoneda f) where
  pure a = Yoneda (\f -> pure (f a))
  Yoneda m <*> Yoneda n = Yoneda (\f -> m (f .) <*> n id)

optimizeFmap :: (a -> b) -> [a] -> Yoneda [] b
optimizeFmap f xs = Yoneda (\g -> fmap (g . f) xs)

myYoneda :: [Int] -> Yoneda [] [Int]
myYoneda xs = pure xs

mainer :: IO ()
mainer = do
  let optimized = optimizeFmap (+ 1) [1, 2, 3]
  print $ runYoneda (fmap (+ 2) optimized) id

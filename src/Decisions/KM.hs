{-# LANGUAGE RankNTypes #-}

module Decisions.KM
( km
, classify
) where

import Decisions.Utils (sample)
import Decisions.Types
  ( Cluster
  , Set
  , Point
  )
import Data.Random (MonadRandom)
import Data.List
  ( nub
  , elemIndices
  , transpose
  )

classify
  :: (RealFrac a, Ord a, Floating a)
  => [[a]]
  -> Point
  -> Point
classify (m:ms) example = map round (nearest example' m ms)
  where example' = map fromIntegral example

km
  :: (MonadRandom m, Ord a, Floating a)
  => [[Integer]]
  -> a
  -> Int
  -> m [[a]]
km datapoints moe clusters = compute <$> sample clusters datapoints'
  where compute = km' datapoints' moe clusters
        datapoints'
          :: (Ord a, Floating a)
          => [[a]]
        datapoints' = (map.map) fromIntegral datapoints

km'
  :: (Ord a, Floating a)
  => [[a]]
  -> a
  -> Int
  -> [[a]]
  -> [[a]]
km' points moe cluster (mean:means)
  | converged = clusters
  | otherwise = km' points moe cluster means'
    where converged = has_converged moe means' $ mean:means
          means'    = [ let s = [ points !! idx | idx <- indices c ] in mean' s | c <- nub clusters ]
          indices c = elemIndices c clusters
          clusters  = map (\p -> nearest p mean means) points
          mean' set = map ((1/size) *) $ map sum $ transpose set
            where size = fromIntegral $ length set

has_converged
  :: (Ord a, Floating a)
  => a
  -> [[a]]
  -> [[a]]
  -> Bool
has_converged _ [] _ = True
has_converged _ _ [] = True
has_converged moe (x:xs) (y:ys)
  | (d <= moe && d >= -moe) = has_converged moe xs ys
  | otherwise               = False
    where d = euclidean x y

nearest
  :: (Ord a, Floating a)
  => [a]
  -> [a]
  -> [[a]]
  -> [a]
nearest _ n [] = n
nearest p n (m:ms)
  | euclidean p m < euclidean p n = nearest p m ms
  | otherwise                     = nearest p n ms

euclidean
  :: (Floating a)
  => [a]
  -> [a]
  -> a
euclidean a b = sqrt $ foldl (\acc (a', b') -> (a' - b')^2 + acc) 0.0 $ zip a b

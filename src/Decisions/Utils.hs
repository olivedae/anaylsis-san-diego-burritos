module Decisions.Utils
( (.:)
, isempty
, get
, sample
, orderByClass
, split
, commonClassOf
, ispure
, largestGain
, largest
, gain
, entropy
, entropy'
, add
, (%)
) where

import Decisions.Types
import Data.Map
  ( Map
  , fromList
  , toList
  , adjust
  )
import qualified Data.Random.Extras as Sample (sample)
import Data.Random
  ( MonadRandom
  , runRVar
  , StdRandom(..)
  )
import Control.Applicative

(.:)
  :: (Functor f1, Functor f)
  => (a -> b)
  -> f (f1 a)
  -> f (f1 b)
(.:) = fmap.fmap

isempty
  :: [a]
  -> Bool
isempty [] = True
isempty xs = False

get
  :: Point
  -> Attribute
  -> Class
get p a = p !! fromIntegral a

sample :: (MonadRandom m) => Int -> [t] -> m [t]
sample s xs = sampled
  where sampled = runRVar (Sample.sample s xs) StdRandom

orderByClass
  :: Set
  -> Field
  -> [(Class, Set)]
orderByClass set field@(_, classes) = getZipList groups
  where groups = (,) <$> ZipList classes <*> ZipList (split set field)

split
  :: Set
  -> Field
  -> [Set]
split set (attribute, classes) = map snd sets
  where sets      = toList $ foldr parse empty set
        parse p d = adjust (p:) (get p attribute) d
        empty     = fromList $ map new classes
        new c     = (c, [])

commonClassOf
  :: Set
  -> Field
  -> Class
commonClassOf set (attr, classes) = c
  where (_, c) = foldl largest ratio ratios
        (ratio:ratios) = [ ((%) set (length set) attr c, c) | c <- classes ]

ispure
  :: Set
  -> Field
  -> Bool
ispure = (.:) (0 ==) entropy

largestGain
  :: Set
  -> [Field]
  -> Field
  -> Field
largestGain set fields for = field
  where (_, field) = foldl largest canidate canidates
        (canidate:canidates) = [ (gain set f for, f) | f <- fields ]

largest
  :: (Ord a, Floating a)
  => (a, b)
  -> (a, b)
  -> (a, b)
largest current@(currentAmt,_) canidate@(canidateAmt,_)
  | canidateAmt > currentAmt = canidate
  | otherwise = current

gain
  :: (Ord a, Floating a)
  => Set
  -> Field
  -> Field
  -> a
gain set field@(fA, fCs) target@(tA, tCs) = e - e'
  where e  = entropy set target
        e' = add [ calculate s | s <- orderByClass set field ]
        calculate (c, sub)
          = (%) sub (length set) fA c * entropy sub target

entropy
  :: (Ord a, Floating a)
  => Set
  -> Field
  -> a
entropy set (attr, classes) = add [ entropy' set attr c | c <- classes ]

entropy'
  :: (Ord a, Floating a)
  => Set
  -> Attribute
  -> Class
  -> a
entropy' set attribute c
  | ratio /= 0 = abs $ ratio * logBase 2 ratio
  | otherwise  = 0
      where ratio = (%) set (length set) attribute c

add
  :: Num a
  => [a]
  -> a
add = foldl (+) 0

(%)
  :: (Ord a, Floating a)
  => Set
  -> Int
  -> Attribute
  -> Class
  -> a
(%) set size attr c = fromIntegral in' / fromIntegral of'
  where in' = length $ filter (\p -> get p attr == c) set
        of' = toInteger size

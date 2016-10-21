module Decisions.ID3
( prune
, id3
, commonClassOf
, isempty
, ispure
, (.:)
, split
, largestGain
, largest
, gain
, orderByClass
, entropy
, entropy'
, add
, (%)
, get
) where

import Decisions
import Control.Applicative
import qualified Data.Map as Map
import Data.List (delete)

prune
  :: a
  -> a
prune = id

id3
  :: DataSet
  -> DecisionTree
id3 (set,fs,tt@(att,cstt))
  | ispure set tt = Leaf (att, get (head set) att)
  | isempty fs    = Leaf (att, commonClassOf set tt)
  | otherwise     = Node a [ Decision (get (head s) a) $ id3 (s,fs',tt) | s <- sets ]
    where f@(a,cs) = largestGain set fs tt
          sets     = split set f
          fs'      = delete f fs

commonClassOf
  :: Set
  -> Field
  -> Class
commonClassOf set (attr, classes) = c
  where (_, c) = foldl largest ratio ratios
        (ratio:ratios) = [ ((%) set (length set) attr c, c) | c <- classes ]

isempty
  :: [a]
  -> Bool
isempty [] = True
isempty xs = False

ispure
  :: Set
  -> Field
  -> Bool
ispure = (.:) (0 ==) entropy

(.:)
  :: (Functor f1, Functor f)
  => (a -> b)
  -> f (f1 a)
  -> f (f1 b)
(.:) = fmap.fmap

split
  :: Set
  -> Field
  -> [Set]
split set field@(attribute, classes) = map snd sets
  where sets      = Map.toList $ foldr parse empty set
        parse p d = Map.adjust (p:) (get p attribute) d
        empty     = Map.fromList $ orderByClass [] field

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

orderByClass
  :: Set
  -> Field
  -> [(Class, Set)]
orderByClass set field@(_, classes) = getZipList groups
  where groups = (,) <$> ZipList classes <*> ZipList (split set field)

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

get
  :: Point
  -> Attribute
  -> Class
get p a = p !! fromIntegral a

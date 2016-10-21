module Decisions.ID3
( id3
, prune
, split
, largestGain
, largest
, isempty
, ispure
, (.:)
, gain
, entropy
, entropy'
, (%)
, add
, get
) where

import Decisions
import Control.Applicative
import qualified Data.Map as Map
import Data.List (delete)

data DecisionTree =
    Node Attribute [Decision]
  | Leaf Pair
  deriving (Show, Read)

data Decision = Decision Class DecisionTree
  deriving (Show, Read)

prune
  :: a
  -> a
prune = id

id3
  :: DataSet
  -> DecisionTree
id3 (set,fs,tt@(att,cstt))
  -- no examples are left
  -- | isempty set =
  | ispure set tt = Leaf (att, get (head set) att)
  | isempty fs    = Leaf (tt, commonClassOf set tt)
  | otherwise     = Node a [ Decision (get (head s) a) $ id3 (s,fs',tt) | s <- sets ]
    where f@(a,cs) = largestGain set fs tt
          sets     = split set f
          fs'      = delete fs f
\
onDecision
  :: DataSet
  -> Attribute
  -> Decision
onDecision ds attribute = Decision class $ id3 ds

commonClassOf
  :: Set
  -> Field
  -> Class
commonClassOf set (attr, classes) = c
  where (_, c) = foldl largest ratio ratios
        (ratio:ratios) = [ ((%) set total attr c, c) | c <- classes ]
        total = (toInteger . length) set

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
split set (attribute, classes) = [ s | (_,s) <- split' ]
  where split'    = Map.toList $ foldr parse dict set
        parse p d = Map.adjust (p:) (get p attribute) d
        dict      = convert $ (,) <$> ZipList classes <*> empty
        convert   = (Map.fromList . getZipList)
        empty     = (ZipList . repeat) []

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
        e' = add [ calculate s | s <- getZipList subsets ]
        subsets = (,) <$> ZipList fCs <*> ZipList (split set field)
        total = (toInteger . length) set
        calculate (c, sub) =
            (%) sub total fA c * entropy sub target

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
      where ratio = (%) set total attribute c
            total = (toInteger . length) set

add
  :: Num a
  => [a]
  -> a
add = foldl (+) 0

(%)
  :: (Ord a, Floating a)
  => Set
  -> Integer
  -> Attribute
  -> Class
  -> a
(%) set size attr c = fromIntegral in' / fromIntegral of'
  where in' = length $ filter (\p -> get p attr == c) set
        of' = size

get
  :: Point
  -> Attribute
  -> Class
get p a = p !! fromIntegral a

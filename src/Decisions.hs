module Decisions
( DataSet
, Set
, Point
, Class
, Attribute
, Pair
, Field
, DecisionTree (..)
, Decision (..)
, classify
, buildConfusionMatrix
, attribute
, classes
, (.:)
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

type DataSet   = (Set, [Field], Field)
type Set       = [Point]
type Point     = [Integer]
type Class     = Integer
type Attribute = Integer
type Pair      = (Attribute, Class)
type Field     = (Attribute, [Class])

data DecisionTree =
    Node Attribute [Decision]
  | Leaf Pair
  deriving (Show, Read)

data Decision = Decision
  { token :: Class
  , tree  :: DecisionTree
  } deriving (Show, Read)

classify
  :: DecisionTree
  -> Point
  -> Maybe Pair
classify (Leaf field) _ = Just field
classify (Node attribute branches) example
  | isempty branch = Nothing
  | otherwise      = classify (tree $ head branch) example
    where c = get example attribute
          branch = filter (\d -> token d == c) branches

data ConfusionMatrix = ConfusionMatrix
  { attribute :: Attribute
  , classes   :: [Class]
  , matrix    :: Map Class (Map Class Int)
  } deriving (Show, Read)

buildConfusionMatrix
  :: Set
  -> Field
  -> [Class]
  -> ConfusionMatrix
buildConfusionMatrix set field@(attribute, classes) predictions = table
  where cols  = new 0
        empty = new cols
        table = ConfusionMatrix
          attribute
          classes
          (build set classes attribute empty)
        new i = fromList $ map (\c -> (c, i)) classes

build
  :: Set
  -> [Class]
  -> Attribute
  -> Map Class (Map Class Int)
  -> Map Class (Map Class Int)
build [] _ _ m = m
build _ [] _ m = m
build (s:ss) (p:ps) attribute m = build ss ps attribute m'
  where actual = get s attribute
        m'     = adjust (adjust (1+) p) actual m

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

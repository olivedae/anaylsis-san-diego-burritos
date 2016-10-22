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
, sample
, (.:)
, isempty
, get
) where

import Decisions.Prepare

import qualified Data.Random.Extras as Sample (sample)
import qualified Data.Random as Random

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
          branch = filter (\ d -> token d == c) branches

sample :: (Random.MonadRandom m) => Int -> [t] -> m [t]
sample s xs = sampled
  where sampled = Random.runRVar (Sample.sample s xs) Random.StdRandom

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

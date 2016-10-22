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
build (s:ss) (c:cs) attribute m = build ss cs attribute m'
  where actual = get s attribute
        m'     = adjust (adjust (1+) c) actual m

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

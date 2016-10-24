{-# LANGUAGE RankNTypes #-}

module Decisions.Types
( DataSet
, Set
, Point
, Class
, Attribute
, Pair
, Field
, Cluster
, DecisionTree (..)
, Decision (..)
) where

type DataSet   = (Set, [Field], Field, Int, Int)
type Set       = [Point]
type Point     = [Integer]
type Class     = Integer
type Attribute = Integer
type Pair      = (Attribute, Class)
type Field     = (Attribute, [Class])
type Cluster   = forall a . (Ord a, Floating a) => [a]

data DecisionTree =
    Node Attribute [Decision]
  | Leaf Pair
  deriving (Show, Read)

data Decision = Decision
  { token :: Class
  , tree  :: DecisionTree
  } deriving (Show, Read)

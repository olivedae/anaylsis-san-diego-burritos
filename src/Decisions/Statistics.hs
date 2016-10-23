module Decisions.Statistics
( classify
, confusionMatrix
, attribute
, classes
) where

import Decisions.Types
import Decisions.Utils
import Data.Map
  ( Map
  , fromList
  , toList
  , adjust
  )

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

confusionMatrix
  :: Set
  -> Field
  -> [Class]
  -> ConfusionMatrix
confusionMatrix set field@(attribute, classes) predictions = table
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

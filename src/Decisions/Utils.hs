module Decisions.Utils
()
where

import Data.Functor
import Control.Applicative
import qualified Data.Foldable as F
import Data.Monoid

-- type DataSet ([Burrito], AttrField, Attributes)
-- type DataSet = ([Burrito], Integer, Integer)

-- data DecisionTree =
--     Node [DecisionTree]
--   | Leaf FeatureValue
--   | Branch FeatureValue DecisionTree
--   deriving (Show, Read)
--
-- class Feature f where
--   val :: Burrito -> f
--   return :: (Burrito -> f)
--
-- foo
--   :: Burrito
--   -> Feature
-- foo = fmap

-- data DecisionTree = Node { decision :: [DecisionBranch] } | Leaf Attribute
--   deriving (Show, Read)
--
-- data DecisionBranch = { attribute :: Attribute, subtree :: DecisionTree }
--   deriving (Show, Read)
--
-- data Attribute f =
--
-- newtype Classes = Classes { classes :: [Attribute] }
--   deriving (Show, Read)

-- locate
--   :: a
--   -> a
-- locate = id

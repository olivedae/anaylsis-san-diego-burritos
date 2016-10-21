module Decisions
( sample
, DataSet
, Set
, Point
, Class
, Attribute
, Pair
, Field
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


-- sample: a simple wrapper over the random.extra.sample
sample :: (Random.MonadRandom m) => Int -> [t] -> m [t]
sample s xs = sampled
  where sampled = Random.runRVar (Sample.sample s xs) Random.StdRandom

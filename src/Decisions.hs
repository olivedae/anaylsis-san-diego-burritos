module Decisions
( sample
, DataSet
, Set
, Point
, Class
, Attribute
, Field
) where

import Decisions.Prepare

import qualified Data.Random.Extras as Sample (sample)
import qualified Data.Random as Random

type DataSet   = (Set, [Field], Attribute)
type Set       = [Point]
type Point     = [Integer]
type Class     = Integer
type Attribute = Integer
type Field     = (Attribute, [Class])


-- sample: a simple wrapper over the random.extra.sample
sample :: (Random.MonadRandom m) => Int -> [t] -> m [t]
sample s xs = sampled
  where sampled = Random.runRVar (Sample.sample s xs) Random.StdRandom

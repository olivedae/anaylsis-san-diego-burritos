module Decisions
( sample
, DataSet
) where

import Decisions.Prepare

import qualified Data.Random.Extras as Sample (sample)
import qualified Data.Random as Random

type DataSet = ([Burrito], Integer, Integer);

-- sample: a simple wrapper over the random.extra.sample
sample :: (Random.MonadRandom m) => Int -> [t] -> m [t]
sample s xs = sampled
  where sampled = Random.runRVar (Sample.sample s xs) Random.StdRandom

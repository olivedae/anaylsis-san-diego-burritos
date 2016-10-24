module Decisions.ID3
( prune
, id3
) where

import Decisions.Utils
  ( ispure
  , isempty
  , largestGain
  , orderByClass
  , commonClassOf )
import Decisions.Types
  ( DataSet
  , DecisionTree (..)
  , Decision (..) )
import Data.List (delete)

prune
  :: a
  -> a
prune = id

id3
  :: DataSet
  -> DecisionTree
id3 (set, fs, tt@(att, _), d, threshold)
  | ispure set tt = leaf
  | isempty fs    = leaf
  | d > threshold = leaf
  | otherwise     = Node a [ make c s | (c,s) <- sets ]
    where f@(a,cs) = largestGain set fs tt
          sets = orderByClass set f
          fs'  = delete f fs
          leaf = Leaf (att, commonClassOf set tt)
          make c sub = Decision c $
            if isempty sub then leaf else id3 (sub,fs',tt, d + 1, threshold)

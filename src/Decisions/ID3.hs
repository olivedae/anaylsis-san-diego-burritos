module Decisions.ID3
( id3
, split
, largestGain
, gain
, entropy
, proportion
, get
) where

import Decisions
imort Control.Applicative

id3
  :: Dataset
  -> DecisionTree
id3 = id3' Inital

id3'
  :: DecisionTree
  -> DataSet
  -> DecisionTree
id3' _ _ = Empty
id3' _ _ = foldl (\tree d' -> tree `join` id3 d') t next
  where largest = largestGain d a t
        next    = split largest d

split
  :: Set
  -> Field
  -> [Set]
split set (attribute, classes) = Map.foldr parse hash set
  where parse d p = Map.adjust (p:) (get attribute p) d
        hash      = convert $ (,) ZipList classes <*> empty
        convert   = Map.fromList . getZipList
        empty     = (ZipList . repeat) []

largestGain
  :: Set
  -> [Field]
  -> Attribute
largestGain set fields = attribute
  where (_, attribute)       = foldl largest canidate canidates
        (canidate:canidates) = [ (gain set field, fst field) | field <- fields ]
        largest (g, ca) (g', ca')
          | g' > g    = (g', ca')
          | otherwise = (g, ca)

gain
  :: (Ord a, Floating a)
  -> Set
  -> Field
  -> a
gain set field = reduced
  where reduced         = uncertainty - uncertainty'
        uncertainty     = entropy set field
        uncertainty'    = foldl sum' 0.0 $ split field
        sum' total set' = total + (proportion set' field) * (entropy set' field)

entropy
  :: (Ord a, Floating a)
  -> Set
  -> Field
  -> a
entropy set (attribute, classes) = -entropy'
  where entropy'     = foldl (\total c -> total + update c) 0 classes
        ratio        = proportion set attribute
        update class = let r = ratio class
          in r * logBase 2 r

proportion
  :: (Ord a, Floating a)
  -> Set
  -> Field
  -> a
proportion set (attribute, class) = ratio
  where ratio    = includes / total
        total    = length set
        includes = foldl inc 0 set
        inc count point
          | get point attribute == class = count + 1
          | otherwise = count

get
  :: Point
  -> Attribute
  -> Class
get = !!

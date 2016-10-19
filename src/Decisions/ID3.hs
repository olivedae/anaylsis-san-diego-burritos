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
import Control.Applicative
import qualified Data.Map as Map

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
split set (attribute, classes) = [ s | (_,s) <- split' ]
  where split'    = Map.toList $ foldr parse dict set
        parse p d = Map.adjust (p:) (get p attribute) d
        dict      = convert $ (,) <$> ZipList classes <*> empty
        convert   = (Map.fromList . getZipList)
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
  :: Floating a
  => Set
  -> Field
  -> a
gain set field = reduced
  where reduced         = uncertainty - uncertainty'
        uncertainty     = entropy set field
        uncertainty'    = foldl sum' 0.0 $ split field
        sum' total set' = total + (proportion set' field) * (entropy set' field)

entropy
  :: Floating a
  => Set
  -> Field
  -> a
entropy set (attribute, classes) = sum subsets
  where subsets = [ calculate c | c <- classes ]
        calculate c =
          (%) set attribute c * information set attribute c

sum
  :: (Num a)
  => [a]
  -> a
sum = foldl (+) 0

information
  :: Floating a
  => Set
  -> Attribute
  -> Class
  -> a
information p a c = logBase 2 $ (%) p a c

(%)
  :: Floating a
  => Set
  -> Attribute
  -> Class
  -> a
(%) set attr c = sum counts / (fromIntegral . length) set
  where counts = map (\p -> if isin p then 1 else 0) set
        isin p = get p attr == c

get
  :: Point
  -> Attribute
  -> Class
get p a = p !! fromIntegral a

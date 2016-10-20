module Decisions.ID3
( id3
, split
, largestGain
, gain
, entropy
, (%)
, add
, information
, get
) where

import Decisions
import Control.Applicative
import qualified Data.Map as Map

id3
  :: a
  -> a
id3 = id

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
  -> Field
  -> Field
largestGain set fields for = field
  where (_, field) = foldl largest canidate canidates
        (canidate:canidates) = [ (gain set f for, f) | f <- fields ]
        largest current@(currentGain,_) cand@(canidateGain,_)
          | canidateGain > currentGain = cand
          | otherwise = current

gain
  :: Floating a
  => Set
  -> Field
  -> Field
  -> a
gain set field@(a, cs) for = e - e'
  where e  = entropy set field for
        e' = add [ calculate s c | s <- subsets, c <- cs ]
        subsets = split set field
        total = (toInteger . length) set
        calculate sub c =
          (%) sub total a c * entropy sub field for

entropy
  :: Floating a
  => Set
  -> Field
  -> Field
  -> a
entropy set field@(attribute, classes) for = abs entropy'
  where entropy' = add [ calculate s c | c <- classes, s <- split set for ]
        total = (toInteger . length) set
        calculate sub c = information sub total attribute c

add
  :: (Num a)
  => [a]
  -> a
add = foldl (+) 0

information
  :: Floating a
  => Set
  -> Integer
  -> Attribute
  -> Class
  -> a
information set total attribute c
  | ratio /= 0 = ratio * logBase 2 ratio
  | otherwise  = 0
  where ratio = (%) set total attribute c

(%)
  :: Floating a
  => Set
  -> Integer
  -> Attribute
  -> Class
  -> a
(%) set size attr c = instances / total
  where instances =
          (fromIntegral . length) $ filter isin set
        isin p = get p attr == c
        total = fromIntegral size

get
  :: Point
  -> Attribute
  -> Class
get p a = p !! fromIntegral a

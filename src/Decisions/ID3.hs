module Decisions.ID3
( id3
, split
, largestGain
, gain
, entropy
, entropy'
, (%)
, add
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
  :: (Eq a, Floating a)
  => Set
  -> Field
  -> Field
  -> a
gain set field@(fA, fCs) target@(tA, tCs) = e - e'
  where e  = entropy set target
        e' = add [ calculate s | s <- getZipList subsets ]
        subsets = (,) <$> ZipList fCs <*> ZipList (split set field)
        total = (toInteger . length) set
        calculate (c, sub) =
            (%) sub total fA c * entropy sub target

entropy
  :: (Eq a, Floating a)
  => Set
  -> Field
  -> a
entropy set (attribute, classes) = abs e
  where e = add [ entropy' set attribute c | c <- classes ]

entropy'
  :: (Eq a, Floating a)
  => Set
  -> Attribute
  -> Class
  -> a
entropy' set attribute c
  | ratio /= 0 = abs $ ratio * logBase 2 ratio
  | otherwise  = 0
      where ratio = (%) set total attribute c
            total = (toInteger . length) set

add
  :: (Num a)
  => [a]
  -> a
add = foldl (+) 0

(%)
  :: Floating a
  => Set
  -> Integer
  -> Attribute
  -> Class
  -> a
(%) set size attr c = fromIntegral in' / fromIntegral of'
  where in' = length $ filter (\p -> get p attr == c) set
        of' = size

get
  :: Point
  -> Attribute
  -> Class
get p a = p !! fromIntegral a

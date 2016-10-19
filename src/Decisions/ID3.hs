module Decisions.ID3
( id3
, split
, largestGain
, gain
, entropy
, (*%*)
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
largestGain set fields = field
  where (_, field) = foldl largest canidate canidates
        (canidate:canidates) = [ (gain set f, f) | f <- fields ]
        largest current@(currentGain,_) cand@(canidateGain,_)
          | canidateGain > currentGain = cand
          | otherwise = current

gain
  :: Floating a
  => Set
  -> Field
  -> a
gain set field = e - e'
  where e  = entropy set field
        e' = add [ (*%*) s field * entropy s field | s <- split set field ]

entropy
  :: Floating a
  => Set
  -> Field
  -> a
entropy set (attribute, classes) = add subsets
  where subsets = [ calculate c | c <- classes ]
        calculate c =
          (%) set attribute c * information set attribute c

add
  :: (Num a)
  => [a]
  -> a
add = foldl (+) 0

information
  :: Floating a
  => Set
  -> Attribute
  -> Class
  -> a
information p a c = - logBase 2 $ (%) p a c

(*%*)
  :: Floating a
  => Set
  -> Field
  -> a
(*%*) _ (_, []) = 0.0
(*%*) set (a, (c:cs)) = (%) set a c + (*%*) set (a, cs)

(%)
  :: Floating a
  => Set
  -> Attribute
  -> Class
  -> a
(%) set attr c = add counts / (fromIntegral . length) set
  where counts = map (\p -> if isin p then 1 else 0) set
        isin p = get p attr == c

get
  :: Point
  -> Attribute
  -> Class
get p a = p !! fromIntegral a

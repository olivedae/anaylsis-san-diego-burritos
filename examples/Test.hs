{-# LANGUAGE ScopedTypeVariables #-}

import Decisions.Prepare (allFeatures, flattenAll, Burrito)
import Decisions
import qualified Decisions.KM as KM (classify)
import System.IO
import Data.Maybe (fromJust)
import Control.Applicative
import Data.List (concat, intersperse)

openID3Model
  :: FilePath
  -> IO DecisionTree
openID3Model file =
  openFile file ReadMode >>= fmap read . hGetContents

openKMModel
  :: (Read a, Ord a, Floating a)
  => FilePath
  -> IO [[a]]
openKMModel file =
  openFile file ReadMode >>= fmap read . hGetContents

openBurritos
  :: FilePath
  -> IO [Burrito]
openBurritos file =
  openFile file ReadMode >>= fmap read . hGetContents

target
  :: Field
target = (overall, [1, 2, 3])

overall
  :: Attribute
overall = 8

header
  :: String
header = (concat . intersperse ",") fields
  where fields =
          [ "algorithm"
          , "actual"
          , "predicted"
          , "correct" ]

format
  :: String
  -> [Class]
  -> [Class]
  -> Int
  -> String
format alg actual predicted index = (concat . intersperse ",") formated
  where formated =
          [ alg
          , show actual'
          , show predicted'
          , show (actual' == predicted') ]
        actual'    = actual !! index
        predicted' = predicted !! index

main = do
  putStrLn "Testing model"

  id3model <- openID3Model "data/burrito.id3.txt"
  kmmodel <- openKMModel "data/burrito.km.txt"
  burrito <- map (fromJust) . flattenAll allFeatures <$> openBurritos "data/burrito.testing.txt"

  let actual      = map (flip get overall) burrito
      predictions = map (snd . fromJust . classify id3model) burrito
      instances   = [0 .. (length predictions) - 1]
      csv         = map (format "id3" actual predictions) instances

  let predictions' = map ((!! fromInteger overall) . KM.classify kmmodel) burrito
      csv'         = map (format "km" actual predictions') instances

  writeFile "data/compare.csv" $ unlines [header, unlines $ csv ++ csv']

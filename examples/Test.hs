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

header'
  :: String
header' = (concat . intersperse ",") fields
  where fields =
          [ "predicted"
          , "cost"
          , "hunger"
          , "tortilla"
          , "meat"
          , "fillings"
          , "meat to fill"
          , "uniformity"
          , "wrap"
          , "overall" ]

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

format'
  :: DecisionTree
  -> Point
  -> String
format' model example = (concat . intersperse ",") formated
  where formated   = map show (prediction example : example)
        prediction = snd . fromJust . classify model

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

  let testing = [
        [c, h, t, n, f, mf, u, w, o] |
        c <- [1..3],
        h <- [1..3],
        t <- [1..3],
        n <- [1..3],
        f <- [1..3],
        mf <- [1..3],
        u <- [1..3],
        w <- [1..3],
        o <- [1..3] ]
      boundary = map (format' id3model) testing

  writeFile "data/boundary.csv" $ unlines [header', unlines boundary]

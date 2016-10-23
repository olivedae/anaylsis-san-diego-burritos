{-# LANGUAGE ScopedTypeVariables #-}

import Decisions.Prepare (allFeatures, flattenAll, Burrito)
import Decisions
import System.IO
import Data.Maybe (fromJust)
import Control.Applicative
import Data.List (concat, intersperse)

openModel
  :: FilePath
  -> IO DecisionTree
openModel file =
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
  :: [Class]
  -> [Class]
  -> Int
  -> String
format actual predicted index = (concat . intersperse ",") formated
  where formated =
          [ "id3"
          , show actual'
          , show predicted'
          , show (actual' == predicted') ]
        actual'    = actual !! index
        predicted' = predicted !! index

main = do
  putStrLn "Testing model"

  model <- openModel "data/burrito.model.txt"
  burrito <- map (fromJust) . flattenAll allFeatures <$> openBurritos "data/burrito.testing.txt"

  let actual      = map (flip get overall) burrito
      predictions = map (snd . fromJust . classify model) burrito
      instances   = [0 .. (length predictions) - 1]
      csv         = map (format actual predictions) instances

  writeFile "data/compare.csv" $ unlines [header, unlines csv]

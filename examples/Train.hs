{-# LANGUAGE ScopedTypeVariables #-}

import Decisions.Prepare
import System.IO
import Control.Exception
import Data.Maybe
import Decisions
import Decisions.ID3
import Decisions.KM
import Data.Map as Map
  ( Map
  , fromList
  , lookup
  )

openBurritos
  :: FilePath
  -> IO [Burrito]
openBurritos file =
  openFile file ReadMode >>= fmap read . hGetContents

fields
  :: [Field]
fields =
  [ (0, [1, 2, 3])
  , (1, [1, 2, 3])
  , (2, [1, 2, 3])
  , (3, [1, 2, 3])
  , (4, [1, 2, 3])
  , (5, [1, 2, 3])
  , (6, [1, 2, 3])
  , (7, [1, 2, 3]) ]

target
  :: Field
target = (8, [1, 2, 3])

type AttributeLabel = String
type ClassLabels    = Map Integer String

toHtml
  :: [(AttributeLabel, ClassLabels)]
  -> DecisionTree
  -> String
toHtml labels (Leaf (a, c)) = "<leaf class='" ++ (show c) ++ "' title='" ++ label ++ "'</leaf>"
  where (Just label) = Map.lookup c (snd $ labels !! fromInteger a)
toHtml labels (Node attribute decisions) = "<node attribute='" ++ (show attribute) ++ "' title='" ++ title ++ "'>" ++ display decisions  ++ "</node>"
  where title = fst $ labels !! fromInteger attribute
        display []     = ""
        display (d:ds) = "<decision class='" ++ show (token d) ++ "' title='" ++ label (token d) ++ "'>" ++ toHtml labels (tree d) ++ "</decision>" ++ display ds
        label c = fromJust $ Map.lookup c (snd $ labels !! fromInteger attribute)

modelLabels
  :: [(AttributeLabel, ClassLabels)]
modelLabels =
  [ ("Cost", scores)
  , ("Hunger", scores)
  , ("Tortilla", scores)
  , ("Meat", scores)
  , ("Fillings", scores)
  , ("Meat to fill ratio", scores)
  , ("Uniformity", scores)
  , ("Wrap", scores)
  , ("Overall", scores) ]
    where scores = fromList $ [(1, "Low"), (2, "Average"), (3, "High")]

main = do
  putStrLn "Training model"

  -- Gather together processed dataset for training
  burrito <- openBurritos "data/burrito.training.txt"

  -- Convert data types in accordance to id3
  let dataset = fromJust <$> flattenAll allFeatures burrito

  -- Create a trained model
  let id3model = id3 (dataset, fields, target, 0, 4)
  kmmodel <- km dataset 0.0001 7

  -- and save
  writeFile "data/burrito.id3.txt" (show id3model)
  writeFile "data/burrito.km.txt" (show kmmodel)
  writeFile "app/tree.html" $ toHtml modelLabels id3model

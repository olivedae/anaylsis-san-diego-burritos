{-# LANGUAGE ScopedTypeVariables #-}

import Decisions.Prepare
import System.IO
import Control.Exception
import Data.Maybe
import Decisions
import Decisions.ID3
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
toHtml labels (Leaf (a, c)) = "<div class='leaf c" ++ (show c) ++ "'><div class='spine'></div>" ++ label ++ "</div>"
  where (Just label) = Map.lookup c (snd $ labels !! fromInteger a)
toHtml labels (Node attribute decisions) = "<div class='node'><div class='spine'></div><div class='attribute'>" ++ (fst $ labels !! fromInteger attribute) ++ "</div><div class='decisions'>" ++ display decisions ++ "</div></div>"
  where display (d:ds) = "<div class='decision'><div class='spine'></div><div class='class'>" ++ label (token d) ++ "</div>" ++ toHtml labels (tree d) ++ "</div>" ++ display ds
        display []     = ""
        label c = fromJust $ Map.lookup c (snd $ labels !! fromInteger attribute)

modelLabels
  :: [(AttributeLabel, ClassLabels)]
modelLabels =
  [ ("cost", scores)
  , ("hunger", scores)
  , ("tortilla", scores)
  , ("meat", scores)
  , ("fillings", scores)
  , ("meat to fill", scores)
  , ("uniformity", scores)
  , ("wrap", scores)
  , ("overall", scores) ]
    where scores = fromList $ [(1, "low"), (2, "avg"), (3, "high")]

main = do
  putStrLn "Training model"

  -- Gather together processed dataset for training
  burrito <- openBurritos "data/burrito.training.txt"

  -- Convert data types in accordance to id3
  let dataset = fromJust <$> flattenAll allFeatures burrito

  -- Create a trained model
  let model = id3 (dataset, fields, target)

  -- and save
  writeFile "data/burrito.model.txt" (show model)
  writeFile "data/tree.html" $ toHtml modelLabels model

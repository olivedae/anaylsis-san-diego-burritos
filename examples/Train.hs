{-# LANGUAGE ScopedTypeVariables #-}

import Decisions.Prepare
import System.IO
import Control.Exception
import Data.Maybe
import Decisions
import Decisions.ID3

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

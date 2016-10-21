{-# LANGUAGE ScopedTypeVariables #-}

import Decisions.Prepare
import System.IO
import Control.Exception
import Data.Maybe
import Decisions
import Decisions.ID3

toBurrito
  :: String
  -> [Burrito]
toBurrito = read

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

main = do
  putStrLn "Training model"

  handle <- openFile "data/burrito.training.txt" ReadMode

  burrito <- toBurrito <$> hGetContents handle

  let dataset = fromJust <$> flattenAll allFeatures burrito
  
  let target = (8, [1, 2, 3])

  let model = id3 (dataset, fields, target)

  writeFile "data/burrito.model.txt" (show model)

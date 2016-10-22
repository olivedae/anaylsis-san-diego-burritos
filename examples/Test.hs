{-# LANGUAGE ScopedTypeVariables #-}

import Decisions.Prepare (allFeatures, flattenAll, Burrito)
import Decisions
import System.IO
import Data.Maybe (fromJust)

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
target = (8, [1, 2, 3])

main = do
  putStrLn "Testing model"

  model <- openModel "data/burrito.model.txt"
  burrito <- flattenAll allFeatures <$> openBurritos "data/burrito.testing.txt"

  let predictions = map (fromJust . \b -> b >>= classify model >>= return . snd) burrito

  let confusion = buildConfusionMatrix
    (map fromJust burrito)
    target
    predictions

  print confusion

{-# LANGUAGE ScopedTypeVariables #-}

import Decisions.Prepare (allFeatures, Burrito)
import Decisions
import System.IO

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

main = do
  putStrLn "Testing model"

  model <- openModel "data/burrito.model.txt"
  burrito <- openBurritos "data/burrito.testing.txt"

  return ()

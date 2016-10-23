import System.Environment (getArgs)

import Decisions.Prepare
import Decisions (sample)
import System.IO
import Data.List ((\\))

computeSizes :: (String, String) -> (Int, Int)
computeSizes (total, training) = (convert total', convert training')
  where total'    = read total :: Double
        training' = total' * (read training :: Double)
        convert v = fromInteger $ round v

main = do
  putStrLn "Prepping datasets"

  burrito <- (fmap $ clean . toList) <$> open "data/burrito.csv"

  case burrito of
    Left reason -> do
      putStrLn "Error preparing datasets"
      print reason
    Right burrito' -> do

      -- Gather selected sizes
      [total, training] <- getArgs

      let (total', training') = computeSizes (total, training)

      -- Seperate out new datasets
      cleanAll <- sample total' burrito'
      cleanTraining <- sample training' cleanAll
      let cleanTesting = cleanAll \\ cleanTraining

      let fTraining = "data/burrito.training.txt"
          fTesting  = "data/burrito.testing.txt"
          fAll      = "data/burrito.all.txt"

      store fTraining cleanTraining
      store fTesting  cleanTesting
      store fAll      cleanAll

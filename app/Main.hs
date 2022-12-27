module Main (main) where

import qualified Data.Vector as V

import DecisionTree
import Control.Parallel.Strategies
import Control.Monad.Par as P

main :: IO ()
main = do
    fileName <- return "iris.txt"
    contents <- readFile fileName
    let 
        contentLines = lines contents
        cleanLines = map cleanLine contentLines `using` parList rpar
        inputData = V.fromList cleanLines
        size = length contentLines -- O(1) operation
        training_idx = 4 * (div size 5)
        training_set = V.slice 0 training_idx inputData -- Training data is the first 80%
        testing_set = V.slice training_idx (size-training_idx) inputData -- Testing data is the last 20%

        partitionIndices = [0,20,40,60,80,100]
        features = [[a,b] | a <- [0,1,2,3], b <- [0,1,2,3], a /= b]
        samples = P.runPar $ P.parMap (partition training_set 40) partitionIndices
        --samples = map (partition training_set 40) partitionIndices
        trainingPairs = zip samples features

        forest = map (\(s,f) -> train s Nothing f) trainingPairs `using` parList rpar
        --forest = map (\(s,f) -> train s Nothing f) trainingPairs
        predictions = P.runPar $ P.parMap (\x -> evaluate forest x) testing_set
        solutions = P.runPar $ P.parMap (\x -> (V.!) x 4) testing_set
    putStrLn (show size)
    putStrLn "Predictions:"
    mapM_ (putStrLn . show) predictions
    putStrLn "Answers:"
    mapM_ (putStrLn . show) solutions

    -- Now we have to evaluate the forest.
    putStrLn "Done."
        

-- Creates a partition of the data rows. O(1) operation.
partition :: (V.Vector(V.Vector Double)) -> Int -> Int 
    -> (V.Vector(V.Vector Double))
partition rows size start = V.slice start size rows

-- Turns the input string array into a Vector of Vectors of Doubles.
cleanup :: [String] -> [V.Vector(Double)]
cleanup []     = []
cleanup (r:rs) = row:(cleanup rs)
    where row = V.fromList (map (read :: String -> Double) splitR)
          splitR = splitComma r

cleanLine :: String -> V.Vector(Double)
cleanLine r = V.fromList (map (read :: String -> Double) (splitComma r))

-- A modification of the prelude 'words' method
splitComma   :: String -> [String]
splitComma s =  case dropWhile (\x -> (x == ',')) s of
                      "" -> []
                      s' -> w : splitComma s''
                            where (w, s'') = break (\x -> (x == ',')) s'


{-
module Main (main) where

import qualified Data.Vector as V

import DecisionTree
import Control.Parallel.Strategies

main :: IO ()
main = do
    fileName <- return "iris.txt"
    contents <- readFile fileName
    let 
        inputData = (V.fromList . cleanup . lines) contents
        training_set = V.slice 0 80 inputData -- Training data is the first 80 rows
        testing_set = V.slice 80 20 inputData -- Testing data is the last 20 rows

        partitionIndices = [0,10,20,30,40,50]
        features = [[a,b] | a <- [0,1,2,3], b <- [0,1,2,3]]
        samples = fmap (partition training_set 30) partitionIndices
        trainingPairs = zip samples features

        forest = fmap (\(s,f) -> train s Nothing f) trainingPairs
        predictions = fmap (\x -> evaluate forest x) testing_set
        solutions = fmap (\x -> (V.!) x 4) testing_set
        
    putStrLn "Predictions:"
    mapM_ (putStrLn . show) predictions

    putStrLn "Answers:"
    mapM_ (putStrLn . show) solutions
        

-- Creates a partition of the data rows. O(1) operation.
partition :: (V.Vector(V.Vector Double)) -> Int -> Int 
    -> (V.Vector(V.Vector Double))
partition rows size start = V.slice start size rows

-- Turns the input string array into a Vector of Vectors of Doubles.
cleanup :: [String] -> [V.Vector(Double)]
cleanup []     = []
cleanup (r:rs) = row:(cleanup rs)
    where row = V.fromList (map (read :: String -> Double) splitR)
          splitR = splitComma r

-- A modification of the prelude 'words' method
splitComma   :: String -> [String]
splitComma s =  case dropWhile (\x -> (x == ',')) s of
                      "" -> []
                      s' -> w : splitComma s''
                            where (w, s'') = break (\x -> (x == ',')) s'
-}
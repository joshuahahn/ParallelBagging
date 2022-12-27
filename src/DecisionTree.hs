module DecisionTree where

import qualified Data.Vector as V
import Control.Monad.Par as P

data DecisionTree a b
    = Threshold a Int (DecisionTree a b) (DecisionTree a b) 
    | Leaf b
    deriving Show

train :: (V.Vector (V.Vector Double)) -> (Maybe Double) -> [Int] -> (DecisionTree Double Int)
train rows threshold (f1:f2:[]) = case threshold of
    -- Initialize the separator to be the mean of the features.
    Nothing -> train rows (averageOf rows f1) [f1,f2]
    Just x  -> Threshold x f1 (train rows1 Nothing [f2]) (train rows2 Nothing [f2])
        where (rows1, rows2) = rowSplit rows x f1

train rows threshold (f2:[]) = case threshold of
    Nothing -> train rows (averageOf rows f2) [f2]
    Just x  -> Threshold x f2 (train rows1 Nothing []) (train rows2 Nothing [])
        where (rows1, rows2) = rowSplit rows x f2

train rows _ ([]) = Leaf (majority rows)

classify :: (V.Vector Double) -> (DecisionTree Double Int) -> Int
classify row classifier = case classifier of
    Threshold val feature leftTree rightTree 
        | ((V.!) row feature) < val -> classify row leftTree
        | otherwise                 -> classify row rightTree
    Leaf category                   -> category

evaluate :: [DecisionTree Double Int] -> (V.Vector Double) -> Int
evaluate classifiers row 
    | ones >= 3 = 1
    | otherwise = 2
    where ones = length $ filter (==1) results
          results = map (classify row) classifiers `using` parList rpar

-- Decides the majority vote on the feature.
majority :: (V.Vector (V.Vector Double)) -> Int
majority rows 
    | 2 * ones > V.length rows = 1
    | otherwise           = 2
    where 
        ones = V.length $ (V.filter (\x -> ((V.!) x 4) == 1)) rows

-- Splits the dataset on the threshold and feature.
rowSplit :: (V.Vector (V.Vector Double)) -> Double -> Int
    -> ((V.Vector (V.Vector Double)),  (V.Vector (V.Vector Double)))
rowSplit rows threshold feature = (r1, r2) 
    where 
        r1 = V.filter (\x -> ((V.!) x feature) < threshold) rows
        r2 = V.filter (\x -> ((V.!) x feature) >= threshold) rows

-- Calculates the average value of the feature.
averageOf :: V.Vector (V.Vector Double) -> Int -> (Maybe Double)
averageOf rows feature = Just (total / (fromIntegral $ V.length rows))
    where total = V.foldl (\x y -> x + (V.!) y feature) 0 rows
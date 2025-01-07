module SumOfMultiples (sumOfMultiples) where

import Data.Containers.ListUtils (nubOrd)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum deduplicated
  where
    multipliers factor
      | factor > 0 = takeWhile (< limit) $ iterate (+ factor) factor
      | otherwise = []
    deduplicated = nubOrd $ concatMap multipliers factors

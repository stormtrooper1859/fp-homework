module Part1
  ( contains
  , order3
  , smartReplicate
  , stringSum
  ) where

import Part2 (mergeSort)

-- task 1
order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (a, b, c) = let [x, y, z] = mergeSort [a, b, c] in (x, y, z)

-- task 2
smartReplicate :: [Int] -> [Int]
smartReplicate []       = []
smartReplicate (x : xs) = repeater (smartReplicate xs) x x

repeater :: [Int] -> Int -> Int -> [Int]
repeater list _ 0      = list
repeater list num left = num : repeater list num (left - 1)

-- task 3
contains :: Eq a => a -> [[a]] -> [[a]]
contains _ []         = []
contains num (x : xs)
  | elem num x = x : contains num xs
  | otherwise  = contains num xs

-- task 4
stringSum :: String -> Int
stringSum = sum . map read . words

module Part2
  ( mergeSort
  , randomIntList
  , removeAt
  ) where

import           System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

-- task 1
removeAt :: Eq a => Int -> [a] -> ([a], a)
removeAt = undefined

-- task 2
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort (x : []) = [x]
mergeSort list =
  let (a, b) = splitAt (div (length list) 2) list
  in merge (mergeSort a) (mergeSort b)

merge :: Ord a => [a] -> [a] -> [a]
merge a []  = a
merge [] b  = b
merge a@(ah : at) b@(bh : bt)
  | ah < bh   = ah : merge at b
  | otherwise = bh : merge a bt

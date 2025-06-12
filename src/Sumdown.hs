module Sumdown (sumdown) where

sumdown :: Int -> Int
sumdown x
  | x < 0 = error "input must be a non-negative integer"
  | x == 0 = 0
  | otherwise = x + sumdown (x - 1)

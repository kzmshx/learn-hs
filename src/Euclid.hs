module Euclid (euclid, euclid2) where

euclid :: Int -> Int -> Int
euclid a b
  | a < 0 || b < 0 = error "inputs must be non-negative"
  | a == 0 = b
  | b == 0 = a
  | a == b = a
  | a < b = euclid a (b - a)
  | otherwise = euclid b (a - b)

euclid2 :: Int -> Int -> Int
euclid2 a b
  | a < 0 || b < 0 = error "inputs must be non-negative"
  | b == 0 = a
  | otherwise = euclid b (a `mod` b)

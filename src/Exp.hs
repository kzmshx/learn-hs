module Exp (myExp) where

myExp :: Int -> Int -> Int
myExp _ 0 = 1
myExp 0 e
  | e < 0 = error "exponent must be non-negative when base is 0"
  | otherwise = 0
myExp b e
  | b < 0 = error "base must be non-negative"
  | otherwise = b * myExp b (e - 1)

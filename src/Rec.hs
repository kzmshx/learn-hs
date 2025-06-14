{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
module Rec (euclid, euclid2, myAnd, myConcat, myElem, myExp, myNth, myReplicate, sumdown) where

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

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x : xs) = x && myAnd xs

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x : xs) = x ++ myConcat xs

myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem e (x : xs) = e == x || myElem e xs

myExp :: Int -> Int -> Int
myExp _ 0 = 1
myExp 0 e
  | e < 0 = error "exponent must be non-negative when base is 0"
  | otherwise = 0
myExp b e
  | b < 0 = error "base must be non-negative"
  | otherwise = b * myExp b (e - 1)

myNth :: [a] -> Int -> a
myNth list 0 = head list
myNth list n = myNth (tail list) (n - 1)

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n a = a : myReplicate (n - 1) a

sumdown :: Int -> Int
sumdown x
  | x < 0 = error "input must be a non-negative integer"
  | x == 0 = 0
  | otherwise = x + sumdown (x - 1)

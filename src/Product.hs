{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
module Product (product) where

import Prelude hiding (product)

product :: (Num a) => [a] -> a
product [] = 1
product (x : xs) = x * product xs

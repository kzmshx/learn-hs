{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module MyList (myInit, myLast, halve, thirdByHeadTail, thirdByIndex, thirdByMatch) where

myInit :: [a] -> [a]
myInit [] = error "empty list"
myInit x = take (length x - 1) x

myLast :: [a] -> a
myLast [] = error "empty list"
myLast x = x !! (length x - 1)

-- Implement halve using prelude functions
halve :: [a] -> ([a], [a])
halve x = splitAt (length x `div` 2) x

-- Implement `third` using `head` and `tail`
thirdByHeadTail :: [a] -> a
thirdByHeadTail [] = error "list must be at least length 3"
thirdByHeadTail [_] = error "list must be at least length 3"
thirdByHeadTail [_, _] = error "list must be at least length 3"
thirdByHeadTail x = head (tail (tail x))

-- Implement `third` using index access
thirdByIndex :: [a] -> a
thirdByIndex [] = error "list must be at least length 3"
thirdByIndex [_] = error "list must be at least length 3"
thirdByIndex [_, _] = error "list must be at least length 3"
thirdByIndex x = x !! 2

-- Implement `third` using pattern match
thirdByMatch :: [a] -> a
thirdByMatch [] = error "list must be at least length 3"
thirdByMatch [_] = error "list must be at least length 3"
thirdByMatch [_, _] = error "list must be at least length 3"
thirdByMatch (_ : _ : x : _) = x

module MyList (myInit, myLast) where

myInit :: [a] -> [a]
myInit [] = error "empty list"
myInit x = take (length x - 1) x

myLast :: [a] -> a
myLast [] = error "empty list"
myLast x = x !! (length x - 1)

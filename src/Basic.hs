module Basic (luhn, myProduct, quadruple) where

luhnDouble :: Int -> Int
luhnDouble x
  | x > 10 = error "luhnDouble: input must not be greater than 10"
  | x > 4 = 2 * x - 9
  | otherwise = 2 * x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0

myProduct :: (Num a) => [a] -> a
myProduct [] = 1
myProduct (x : xs) = x * product xs

quadruple :: (Num a) => a -> a
quadruple x = x * 4

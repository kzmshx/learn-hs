module MyListSpec (spec) where

import Control.Exception (evaluate)
import MyList (myInit, myLast)
import SpecHelper (itEach)
import Test.Hspec (Spec, anyException, describe, it, shouldBe, shouldThrow)

spec :: Spec
spec = do
  describe "myLast" $ do
    itEach
      [ ([1, 2, 3], 3),
        ([1], 1)
      ]
      (\(input, expected) -> "myLast " ++ show input ++ " = " ++ show expected)
      (\(input, expected) -> myLast (input :: [Int]) `shouldBe` expected)

    it "myLast [] throws an error" $ do
      evaluate (myLast []) `shouldThrow` anyException

  describe "myInit" $ do
    itEach
      [ ([1, 2, 3], [1, 2]),
        ([1], [])
      ]
      (\(input, expected) -> "myInit " ++ show input ++ " = " ++ show expected)
      (\(input, expected) -> myInit (input :: [Int]) `shouldBe` expected)

    it "myInit [] throws an error" $ do
      evaluate (myInit []) `shouldThrow` anyException

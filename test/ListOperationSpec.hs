module ListOperationSpec (spec) where

import Control.Exception (evaluate)
import ListOperation (halve, myInit, myLast, safetail1, safetail2, safetail3, thirdByHeadTail, thirdByIndex, thirdByMatch)
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

  describe "halve" $ do
    itEach
      [ ([], ([], [])),
        ([1], ([], [1])),
        ([1, 2], ([1], [2])),
        ([1, 2, 3], ([1], [2, 3])),
        ([1, 2, 3, 4], ([1, 2], [3, 4]))
      ]
      (\(input, expected) -> "halve " ++ show input ++ " = " ++ show expected)
      (\(input, expected) -> halve (input :: [Int]) `shouldBe` expected)

  describe "thirdByHeadTail" $ do
    itEach
      [ ([1, 2, 3], 3),
        ([1, 2, 3, 4], 3),
        ([1, 2, 3, 4, 5], 3)
      ]
      (\(input, expected) -> "thirdByHeadTail " ++ show input ++ " = " ++ show expected)
      (\(input, expected) -> thirdByHeadTail (input :: [Int]) `shouldBe` expected)

  describe "thirdByIndex" $ do
    itEach
      [ ([1, 2, 3], 3),
        ([1, 2, 3, 4], 3),
        ([1, 2, 3, 4, 5], 3)
      ]
      (\(input, expected) -> "thirdByIndex " ++ show input ++ " = " ++ show expected)
      (\(input, expected) -> thirdByIndex (input :: [Int]) `shouldBe` expected)

  describe "thirdByMatch" $ do
    itEach
      [ ([1, 2, 3], 3),
        ([1, 2, 3, 4], 3),
        ([1, 2, 3, 4, 5], 3)
      ]
      (\(input, expected) -> "thirdByMatch " ++ show input ++ " = " ++ show expected)
      (\(input, expected) -> thirdByMatch (input :: [Int]) `shouldBe` expected)

  describe "safetail" $ do
    itEach
      [ ([1], []),
        ([1, 2], [2]),
        ([1, 2, 3], [2, 3])
      ]
      (\(input, expected) -> "safetail " ++ show input ++ " = " ++ show expected)
      ( \(input, expected) -> do
          safetail1 (input :: [Int]) `shouldBe` expected
          safetail2 (input :: [Int]) `shouldBe` expected
          safetail3 (input :: [Int]) `shouldBe` expected
      )

    it "safetail [] = []" $ do
      safetail1 ([] :: [Int]) `shouldBe` []
      safetail2 ([] :: [Int]) `shouldBe` []
      safetail3 ([] :: [Int]) `shouldBe` []

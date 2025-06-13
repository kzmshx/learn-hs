module RecSpec (spec) where

import Control.Exception (evaluate)
import Rec (euclid, euclid2, myAnd, myConcat, myElem, myExp, myNth, myReplicate)
import SpecHelper (itEach)
import Test.Hspec (Spec, anyException, describe, it, shouldBe, shouldThrow)

spec :: Spec
spec = do
  describe "exp" $ do
    itEach
      [ ((0, 0), 1),
        ((1, 0), 1),
        ((2, 1), 2),
        ((2, 2), 4),
        ((2, 3), 8)
      ]
      (\(input, expected) -> "myExp " ++ show input ++ " = " ++ show expected)
      (\((b, e), expected) -> myExp b e `shouldBe` expected)

    it "myExp throws an error when negative value is passed" $ do
      evaluate (myExp (-1) 2) `shouldThrow` anyException

    it "myExp throws an error when base = 0 and exp < 0" $ do
      evaluate (myExp 0 (-1)) `shouldThrow` anyException

  describe "euclid" $ do
    itEach
      [ ((0, 0), 0),
        ((0, 5), 5),
        ((7, 0), 7),
        ((7, 1), 1),
        ((12, 8), 4),
        ((6, 27), 3),
        ((17, 13), 1),
        ((17, 17), 17)
      ]
      (\(input, expected) -> "euclid " ++ show input ++ " = " ++ show expected)
      (\((a, b), expected) -> euclid a b `shouldBe` expected)

  describe "euclid2" $ do
    itEach
      [ ((0, 0), 0),
        ((0, 5), 5),
        ((7, 0), 7),
        ((7, 1), 1),
        ((12, 8), 4),
        ((6, 27), 3),
        ((17, 13), 1),
        ((17, 17), 17)
      ]
      (\(input, expected) -> "euclid2 " ++ show input ++ " = " ++ show expected)
      (\((a, b), expected) -> euclid2 a b `shouldBe` expected)

  describe "myAnd" $ do
    itEach
      [ ([], True),
        ([True], True),
        ([False], False),
        ([True, True], True),
        ([True, False], False),
        ([False, True], False),
        ([False, False], False)
      ]
      (\(input, expected) -> "myAnd " ++ show input ++ " = " ++ show expected)
      (\(input, expected) -> myAnd input `shouldBe` expected)

  describe "myConcat" $ do
    itEach
      [ ([], []),
        ([['h']], "h"),
        ([['h'], ['e', 'l'], ['l', 'o', '!']], "hello!")
      ]
      (\(input, expected) -> "myConcat " ++ show input ++ " = " ++ show expected)
      (\(input, expected) -> myConcat input `shouldBe` expected)

  describe "myReplicate" $ do
    itEach
      [ ((0, 0), []),
        ((1, 0), [0]),
        ((2, 1), [1, 1]),
        ((3, 2), [2, 2, 2])
      ]
      (\(input, expected) -> "myReplicate " ++ show input ++ " = " ++ show expected)
      (\((n, a), expected) -> myReplicate (n :: Int) (a :: Int) `shouldBe` expected)

  describe "myNth" $ do
    itEach
      [ (([1], 0), 1)
      ]
      (\(input, expected) -> "myNth " ++ show input ++ " = " ++ show expected)
      (\((list, n), expected) -> (list :: [Int]) `myNth` n `shouldBe` expected)

    it "myNth throws an error when list is empty" $ do
      evaluate ([] `myNth` 0) `shouldThrow` anyException

  describe "myElem" $ do
    itEach
      [ ((1, []), False),
        ((0, [1]), False),
        ((1, [1]), True),
        ((3, [1, 2]), False),
        ((3, [1, 2, 3]), True)
      ]
      (\(input, expected) -> "myElem " ++ show input ++ " = " ++ show expected)
      (\((e, l), expected) -> e `myElem` l `shouldBe` expected)

module SumdownSpec (spec) where

import Control.Exception (evaluate)
import SpecHelper (itEach)
import Sumdown (sumdown)
import Test.Hspec (Spec, anyException, describe, it, shouldBe, shouldThrow)

spec :: Spec
spec = do
  describe "sumdown" $ do
    itEach
      [ (0, 0),
        (1, 1),
        (5, 15)
      ]
      (\(input, expected) -> "sumdown " ++ show input ++ " = " ++ show expected)
      (\(input, expected) -> sumdown (input :: Int) `shouldBe` expected)

    it "sumdown throws an error when negative value is passed" $ do
      evaluate (sumdown (-1)) `shouldThrow` anyException

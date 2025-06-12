module ExpSpec (spec) where

import Control.Exception (evaluate)
import Exp (myExp)
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

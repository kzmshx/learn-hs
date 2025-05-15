module QuadrupleSpec (spec) where

import Quadruple (quadruple)
import SpecHelper (itEach)
import Test.Hspec (Spec, describe, shouldBe)

spec :: Spec
spec = describe "quadruple" $ do
  describe "with Int" $ do
    itEach
      [ (2, 8),
        (0, 0),
        (-3, -12)
      ]
      (\(input, expected) -> "quadruple " ++ show input ++ " = " ++ show expected)
      (\input expected -> quadruple (input :: Int) `shouldBe` expected)

  describe "with Double" $ do
    itEach
      [ (2.0, 8.0),
        (0.0, 0.0),
        (-3.0, -12.0)
      ]
      (\(input, expected) -> "quadruple " ++ show input ++ " = " ++ show expected)
      (\input expected -> quadruple (input :: Double) `shouldBe` expected)

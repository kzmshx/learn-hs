module BasicSpec (spec) where

import Basic (luhn, myProduct, quadruple)
import SpecHelper (itEach)
import Test.Hspec (Spec, describe, shouldBe)

spec :: Spec
spec = do
  describe "luhn" $ do
    itEach
      [ (1, 7, 8, 4, True),
        (1, 7, 8, 5, False)
      ]
      (\(a, b, c, d, expected) -> "luhn " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d ++ " = " ++ show expected)
      (\(a, b, c, d, expected) -> luhn a b c d `shouldBe` expected)

  describe "myProduct" $ do
    itEach
      ( [ ([1, 2, 3], 6),
          ([1, 2, 3, 4], 24),
          ([1, 2, 3, 4, 5], 120),
          ([], 1)
        ] ::
          [([Int], Int)]
      )
      (\(input, expected) -> "myProduct " ++ show input ++ " = " ++ show expected)
      (\(input, expected) -> myProduct input `shouldBe` expected)

  describe "quadruple" $ do
    describe "with Int" $ do
      itEach
        [ (2, 8),
          (0, 0),
          (-3, -12)
        ]
        (\(input, expected) -> "quadruple " ++ show input ++ " = " ++ show expected)
        (\(input, expected) -> quadruple (input :: Int) `shouldBe` expected)

    describe "with Double" $ do
      itEach
        [ (2.0, 8.0),
          (0.0, 0.0),
          (-3.0, -12.0)
        ]
        (\(input, expected) -> "quadruple " ++ show input ++ " = " ++ show expected)
        (\(input, expected) -> quadruple (input :: Double) `shouldBe` expected)

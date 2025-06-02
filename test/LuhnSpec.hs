module LuhnSpec (spec) where

import Luhn (luhn)
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

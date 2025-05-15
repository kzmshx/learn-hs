module ProductSpec (spec) where

import Product (product)
import SpecHelper (itEach)
import Test.Hspec (Spec, describe, shouldBe)
import Prelude hiding (product)

spec :: Spec
spec = describe "product" $ do
  itEach
    ( [ ([1, 2, 3], 6),
        ([1, 2, 3, 4], 24),
        ([1, 2, 3, 4, 5], 120),
        ([], 1)
      ] ::
        [([Int], Int)]
    )
    (\(input, expected) -> "product " ++ show input ++ " = " ++ show expected)
    (\(input, expected) -> product input `shouldBe` expected)

module ListComprehensionSpec (spec) where

import ListComprehension (quickSort)
import SpecHelper (itEach)
import Test.Hspec (Spec, describe, shouldBe)

spec :: Spec
spec = describe "quickSort" $ do
  itEach
    ( [ ([], []),
        ([1], [1]),
        ([1, 2], [1, 2]),
        ([2, 1], [1, 2]),
        ([3, 1, 2], [1, 2, 3]),
        ([1, 1, 2, 3, 3], [1, 1, 2, 3, 3])
      ] ::
        [([Int], [Int])]
    )
    (\(input, expected) -> "quickSort " ++ show input ++ " = " ++ show expected)
    (\(input, expected) -> quickSort input `shouldBe` expected)

module EuclidSpec (spec) where

import Euclid (euclid, euclid2)
import SpecHelper (itEach)
import Test.Hspec (Spec, describe, shouldBe)

spec :: Spec
spec = do
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

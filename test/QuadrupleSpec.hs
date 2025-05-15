module QuadrupleSpec (spec) where

import Quadruple (quadruple)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "quadruple" $ do
  describe "with Int" $ do
    let cases =
          [ (2, 8),
            (0, 0),
            (-3, -12)
          ]
    let testEach (input, expected) =
          let desc = "Quadruple(" ++ show input ++ ") = " ++ show expected
           in it desc $ quadruple (input :: Int) `shouldBe` expected
    mapM_ testEach cases

  describe "with Double" $ do
    let cases =
          [ (2.0, 8.0),
            (0.0, 0.0),
            (-3.0, -12.0)
          ]
    let testEach (input, expected) =
          let desc = "Quadruple(" ++ show input ++ ") = " ++ show expected
           in it desc $ quadruple (input :: Double) `shouldBe` expected
    mapM_ testEach cases

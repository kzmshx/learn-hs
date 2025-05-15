module QuadrupleSpec (spec) where

import Quadruple (quadruple)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Quadruple" $ do
    it "should return 8 when input is 2" $ do
      quadruple 2 `shouldBe` 8

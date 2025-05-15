module SpecHelper (itEach) where

import Test.Hspec (Expectation, Spec, it)

itEach :: [a] -> (a -> String) -> (a -> Expectation) -> Spec
itEach cases description test =
  mapM_
    (\input -> it (description input) $ test input)
    cases

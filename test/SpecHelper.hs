module SpecHelper (itEach) where

import Test.Hspec (Expectation, Spec, it)

itEach ::
  [(a, b)] ->
  ((a, b) -> String) ->
  (a -> b -> Expectation) ->
  Spec
itEach cases descFn testFn =
  mapM_
    (\(input, expected) -> it (descFn (input, expected)) $ testFn input expected)
    cases

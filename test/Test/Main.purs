module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Discovery (discoverAndRunSpecs)
import Test.Spec.Reporter (consoleReporter)

foreign import setupJsdom :: Effect Unit

main :: Effect Unit
main = do
  setupJsdom
  discoverAndRunSpecs [ consoleReporter ] """Test\.Yoga\.React\.Om\..*Spec"""

module Test.Yoga.React.Om.LayerSpec where

import Prelude

import Effect.Ref (Ref)
import Yoga.Om (toOm)
import Yoga.Om.Ref as Ref
import React.Basic (JSX)
import React.TestingLibrary (cleanup, render)
import Yoga.Om.Test (Spec, after_, describe, it, textContentShouldEqual)
import Yoga.Om.Layer (OmLayer, makeLayer, runLayer, provide)
import Yoga.React.Om (omComponent, useOm)
import Yoga.React.Om as Om
import Yoga.React.DOM.Internal (text)

counterLayer :: OmLayer () () { counter :: Ref Int }
counterLayer = makeLayer do
  counter <- Ref.new 42
  pure { counter }

mkApp :: OmLayer (counter :: Ref Int) () { app :: Unit -> JSX }
mkApp = makeLayer do
  omComponent "CounterDisplay"
    ( \_ -> Om.do
        n <- useOm \{ counter } -> Ref.read counter
        Om.pure $ text ("Count: " <> show n)
    ) <#> { app: _ }

appLayer :: OmLayer () () { app :: Unit -> JSX }
appLayer = mkApp `provide` counterLayer

spec :: Spec Unit
spec = after_ cleanup do
  describe "omComponent + OmLayer" (pure {}) do
    it "wires layers and renders from provided context" do
      { app } <- runLayer {} appLayer
      { findByText } <- render (app unit)
      result <- findByText "Count: 42" # toOm
      result `textContentShouldEqual` "Count: 42"

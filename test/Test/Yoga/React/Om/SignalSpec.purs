module Test.Yoga.React.Om.SignalSpec where

import Prelude

import Effect.Class (liftEffect)
import React.Basic (JSX)
import React.TestingLibrary (cleanup, render)
import Yoga.Om (Om, ask, toOm)
import Yoga.Om.Test (Spec, after_, describe, it, itWith, textContentShouldEqual)
import Yoga.React.Om (omComponent, useCtx, useSignal)
import Yoga.React.Om as Om
import Yoga.React.Om.Signal (Signal, mkSignal)
import Yoga.React.DOM.Internal (text)

spec :: Spec Unit
spec = after_ cleanup do
  describe "useSignal" (mkCtx 42) do
    it "renders initial signal value" do
      comp <- mkCounter
      { findByText } <- render (comp unit)
      result <- findByText "Count: 42" # toOm
      result `textContentShouldEqual` "Count: 42"

    itWith "re-renders when signal is modified" (mkCtx 0) do
      { count: sig } <- ask
      comp <- mkCounter
      { findByText } <- render (comp unit)
      _ <- findByText "Count: 0" # toOm
      liftEffect $ sig.modify (_ + 1)
      result <- findByText "Count: 1" # toOm
      result `textContentShouldEqual` "Count: 1"

    itWith "multiple signal updates" (mkCtx 0) do
      { count: sig } <- ask
      comp <- mkCounter
      { findByText } <- render (comp unit)
      _ <- findByText "Count: 0" # toOm
      sig.modify (_ + 10) # toOm
      _ <- findByText "Count: 10" # toOm
      sig.modify (_ + 5) # toOm
      result <- findByText "Count: 15" # toOm
      result `textContentShouldEqual` "Count: 15"
  where
  mkCtx n = liftEffect $ mkSignal n <#> { count: _ }

mkCounter
  :: forall ctx err
   . Om { count :: Signal Int | ctx } err (Unit -> JSX)
mkCounter = omComponent "SignalCounter" \_ -> Om.do
  { count } <- useCtx
  n <- useSignal count
  Om.pure $ text ("Count: " <> show n)

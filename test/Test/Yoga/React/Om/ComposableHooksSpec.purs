module Test.Yoga.React.Om.ComposableHooksSpec where

import Prelude

import Effect (Effect)
import Effect.Ref (Ref)
import Yoga.Om (ask, toOm)
import Yoga.Om.Ref as Ref
import React.TestingLibrary (cleanup, render)
import Yoga.Om.Test (Spec, after_, describe, it, shouldEqual, textContentShouldEqual)
import Yoga.React.Om (OmRender, UseOm, omComponent, useOm)
import Yoga.React.Om as Om
import Yoga.React.DOM.Internal (text)

useCounter
  :: forall ctx hooks
   . OmRender (counter :: Ref Int | ctx) hooks (UseOm (counter :: Ref Int | ctx) Int hooks) Int
useCounter = useOm \{ counter } -> Ref.read counter

useLogger
  :: forall ctx hooks
   . String
  -> OmRender (logger :: String -> Effect Unit | ctx) hooks (UseOm (logger :: String -> Effect Unit | ctx) Unit hooks) Unit
useLogger msg = useOm \{ logger } -> logger msg

mkDashboard = omComponent "Dashboard" \_ -> Om.do
  count <- useCounter
  useLogger ("Rendered with count: " <> show count)
  Om.pure $ text ("Dashboard: " <> show count)

spec :: Spec Unit
spec = after_ cleanup do
  describe "composable context-slicing hooks" mkCtx do
    it "infers ctx as union of hook requirements" do
      { logRef } <- ask
      dashboard <- mkDashboard
      { findByText } <- render (dashboard unit)
      result <- findByText "Dashboard: 99" # toOm
      result `textContentShouldEqual` "Dashboard: 99"
      logged <- Ref.read logRef
      logged `shouldEqual` "Rendered with count: 99"
  where
  mkCtx = do
    logRef <- Ref.new ""
    counterRef <- Ref.new 99
    pure { counter: counterRef, logger: (\msg -> Ref.write msg logRef), logRef }

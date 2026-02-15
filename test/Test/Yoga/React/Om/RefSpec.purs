module Test.Yoga.React.Om.RefSpec where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import React.Basic (fragment)
import React.TestingLibrary (cleanup, fireEventClick, render)
import Yoga.Om (toOm)
import Yoga.Om.Test (Spec, after_, describe, it)
import Yoga.React.Om (omComponent, useCtx, useState)
import Yoga.React.Om as Om
import Yoga.React.DOM.HTML.Button (button)
import Yoga.React.DOM.Handler (onClick)
import Yoga.React.DOM.Internal (text)

spec :: Spec Unit
spec = after_ cleanup do
  describe "omComponent + Ref interaction" mkCtx do
    it "mutates a Ref from ctx and re-renders" do
      counter <- omComponent "Counter" \_ -> Om.do
        { counter: ctr } <- useCtx
        count /\ setCount <- useState 0
        Om.pure $ fragment
          [ text ("Count: " <> show count)
          , button
              { onClick: onClick \_ -> do
                  n <- Ref.modify (_ + 1) ctr
                  setCount (const n)
              }
              "inc"
          ]
      { findByText } <- render (counter unit)
      _ <- findByText "Count: 0" # toOm
      btn <- findByText "inc" # toOm
      fireEventClick btn
      _ <- findByText "Count: 1" # toOm
      pure unit
  where
  mkCtx = do
    ref <- liftEffect $ Ref.new 0
    pure { counter: ref }

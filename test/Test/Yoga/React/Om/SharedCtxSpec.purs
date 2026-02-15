module Test.Yoga.React.Om.SharedCtxSpec where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Yoga.Om (Om, ask, toOm)
import Yoga.Om.Ref as Ref
import React.Basic (JSX, fragment)
import React.TestingLibrary (cleanup, fireEventClick, render)
import Yoga.Om.Test (Spec, after_, describe, it)
import Yoga.React.Om (omComponent, useCtx, useState, useEffectOnce)
import Yoga.React.Om as Om
import Yoga.React.DOM.HTML.Button (button)
import Yoga.React.DOM.Handler (onClick)
import Yoga.React.DOM.Internal (text)

spec :: Spec Unit
spec = after_ cleanup do
  describe "shared context across components" mkCtx do
    it "two components share a Ref via Om context" do
      { notifyRef } <- ask
      inc /\ display <- do
        i <- mkIncrementer
        d <- mkDisplay
        pure (i /\ d)
      { findByText } <- render (fragment [ inc unit, display unit ])
      _ <- findByText "Value: 0" # toOm
      btn <- findByText "+1" # toOm
      fireEventClick btn
      liftEffect do
        cb <- Ref.read notifyRef
        cb
      _ <- findByText "Value: 1" # toOm
      pure unit
  where
  mkCtx = do
    ref <- Ref.new 0
    notifyRef <- Ref.new (pure unit :: Effect Unit)
    pure { counter: ref, notify: (\cb -> Ref.write cb notifyRef), notifyRef }

mkIncrementer
  :: forall ctx err
   . Om { counter :: Ref Int | ctx } err (Unit -> JSX)
mkIncrementer =
  omComponent "Incrementer" \_props -> Om.do
    { counter } <- useCtx
    Om.pure $ button
      { onClick: onClick \_ -> Ref.modify_ (_ + 1) counter }
      "+1"

mkDisplay
  :: forall ctx err
   . Om { counter :: Ref Int, notify :: (Effect Unit -> Effect Unit) | ctx } err (Unit -> JSX)
mkDisplay =
  omComponent "Display" \_ ->
    Om.do
      { counter, notify } <- useCtx
      n /\ setN <- useState 0
      useEffectOnce do
        notify (Ref.read counter >>= \v -> setN (const v))
        pure mempty
      Om.pure $ text ("Value: " <> show n)

module Test.Yoga.React.Om.OmComponentSpec where

import Prelude

import React.TestingLibrary (cleanup, render)
import Yoga.Om (toOm)
import Yoga.Om.Test (Spec, after_, describe, it, itWith, textContentShouldEqual)
import Yoga.React.Om (omComponent, useCtx)
import Yoga.React.Om as Om
import Yoga.React.DOM.Internal (text)

mkGreeter = omComponent "Greeter" \{ name } -> Om.do
  { greeting, magicNumber } <- useCtx
  Om.pure $ text (greeting <> ", " <> name <> "! You are " <> show magicNumber <> ".")

spec :: Spec Unit
spec = after_ cleanup do
  describe "omComponent" (pure { greeting: "Hello", magicNumber: 42 }) do
    it "captures context and renders it" do
      comp <- mkGreeter
      { findByText } <- render (comp { name: "World" })
      result <- findByText "Hello, World! You are 42." # toOm
      result `textContentShouldEqual` "Hello, World! You are 42."

    itWith "different context produces different output"
      (pure { greeting: "Howdy", magicNumber: 7 })
      do
        comp <- mkGreeter
        { findByText } <- render (comp { name: "Partner" })
        result <- findByText "Howdy, Partner! You are 7." # toOm
        result `textContentShouldEqual` "Howdy, Partner! You are 7."

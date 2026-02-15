module Test.Yoga.React.Om.SSRSpec where

import Prelude

import Effect.Ref (Ref)
import Yoga.Om (Om)
import Yoga.Om.Ref as Ref
import React.Basic (JSX)
import Yoga.Om.Test (Spec, describe, it, itWith, shouldEqual, shouldNotEqual)
import Yoga.React.Om (omComponent, useOm)
import Yoga.React.Om as Om
import Yoga.React.DOM.Internal (text)
import Yoga.React.DOM.Server (renderToString, renderToStaticMarkup)

spec :: Spec Unit
spec = describe "SSR" mkCtx do
  it "renderToString produces HTML with context values" do
    comp <- mkCounter
    let html = renderToString (comp unit)
    html `shouldNotEqual` ""

  itWith "renderToStaticMarkup produces clean HTML"
    (mkCtxWith 7)
    do
      comp <- mkCounter
      let html = renderToStaticMarkup (comp unit)
      html `shouldEqual` "Count: 7"
  where
  mkCtxWith n = Ref.new n <#> { counter: _ }
  mkCtx = mkCtxWith 42

mkCounter
  :: forall ctx err
   . Om { counter :: Ref Int | ctx } err (Unit -> JSX)
mkCounter = omComponent "SSRCounter" \_ -> Om.do
  n <- useOm (Ref.read <<< _.counter)
  Om.pure $ text $ "Count: " <> show n

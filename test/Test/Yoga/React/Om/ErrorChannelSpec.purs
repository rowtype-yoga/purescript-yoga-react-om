module Test.Yoga.React.Om.ErrorChannelSpec where

import Prelude

import Data.Either (Either(..))
import Data.Variant (match)
import React.Basic (JSX)
import Test.Spec (Spec, describe, it)
import Yoga.Om.Test (shouldEqual)
import Yoga.Om as Om
import Yoga.React.Om (omComponent)
import Yoga.React.Om as Om
import Yoga.React.DOM.Internal (text)

type LoadErr r = (loadFailed :: String | r)

spec :: Spec Unit
spec = describe "omComponent error channel" do
  it "propagates typed errors from before component creation" do
    result <- Om.runReader {} failingApp
    case result of
      Left err -> err # match
        { loadFailed: \msg -> msg `shouldEqual` "worker init failed"
        , exception: \_ -> pure unit
        }
      Right _ -> "should have failed" `shouldEqual` "but didn't"

  it "allows error-free construction" do
    result <- Om.runReader {} successApp
    case result of
      Left _ -> "should have succeeded" `shouldEqual` "but didn't"
      Right _ -> pure unit

loadWorker :: forall ctx err. Om.Om { | ctx } (LoadErr err) String
loadWorker = Om.throw { loadFailed: "worker init failed" }

failingApp :: forall ctx err. Om.Om { | ctx } (LoadErr err) (Unit -> JSX)
failingApp = do
  workerUrl <- loadWorker
  omComponent "Never" \_ -> Om.do
    Om.pure $ text workerUrl

successApp :: forall ctx err. Om.Om { | ctx } (LoadErr err) (Unit -> JSX)
successApp = do
  omComponent "OK" \_ -> Om.do
    Om.pure $ text "ok"

module Yoga.React.Om.Signal
  ( Signal
  , mkSignal
  ) where

import Prelude

import Control.Monad.ST.Class (liftST)
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event (Event)
import FRP.Event as Event

type Signal a =
  { read :: Effect a
  , modify :: (a -> a) -> Effect Unit
  , event :: Event a
  }

mkSignal :: forall a. a -> Effect (Signal a)
mkSignal initial = do
  ref <- Ref.new initial
  { event, push } <- Event.create # liftST
  pure
    { read: Ref.read ref
    , modify: \f -> do
        new <- Ref.modify f ref
        push new
    , event
    }

module Yoga.React.Om
  ( OmRender
  , omComponent
  , useCtx
  , useOm
  , UseCtx
  , UseOm
  , liftRender
  , bind
  , discard
  , pure
  , useState
  , useState'
  , useEffect
  , useEffectOnce
  , useEffectAlways
  , useLayoutEffect
  , useLayoutEffectOnce
  , useLayoutEffectAlways
  , useRef
  , useMemo
  , useReducer
  , useId
  , useTransition
  , useDeferredValue
  , module ReExports
  ) where

import Prelude hiding (bind, discard, pure)
import Prelude (bind, discard, pure) as Prelude

import Control.Applicative.Indexed (class IxApplicative)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind, ibind)
import Control.Monad.Indexed (class IxMonad)
import Data.Functor.Indexed (class IxFunctor)
import Data.Newtype (class Newtype, un)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Aff (error, killFiber, launchAff, launchAff_, try)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import React.Basic (JSX)
import React.Basic.Hooks (Reducer, UseState, UseEffect, UseLayoutEffect, UseRef, UseMemo, UseReducer, UseId, UseTransition, UseDeferredValue) as ReExports
import React.Basic.Hooks as Hooks
import React.Basic.Hooks.Internal (Render, unsafeHook, unsafeRenderEffect)
import React.Basic.Hooks.Internal as Render
import Type.Equality (class TypeEquals)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Om (Om)
import Yoga.Om as Om

newtype OmRender :: Row Type -> Type -> Type -> Type -> Type
newtype OmRender ctx x y a = OmRender (Render x y a)

derive instance Newtype (OmRender ctx x y a) _

ctxStash :: Ref.Ref (forall r. { | r })
ctxStash = unsafePerformEffect (Ref.new (unsafeCoerce {}))

toEffect :: forall ctx x y a. OmRender ctx x y a -> Effect a
toEffect = unsafeCoerce

fromEffect :: forall ctx x y a. Effect a -> OmRender ctx x y a
fromEffect = unsafeCoerce

instance IxFunctor (OmRender ctx) where
  imap f m = fromEffect (map f (toEffect m))

instance IxApply (OmRender ctx) where
  iapply f a = fromEffect (apply (toEffect f) (toEffect a))

instance IxApplicative (OmRender ctx) where
  ipure a = fromEffect (Prelude.pure a)

instance IxBind (OmRender ctx) where
  ibind m f = fromEffect (Prelude.bind (toEffect m) \a -> toEffect (f a))

instance IxMonad (OmRender ctx)

instance Functor (OmRender ctx x y) where
  map f m = fromEffect (map f (toEffect m))

instance TypeEquals x y => Apply (OmRender ctx x y) where
  apply f a = fromEffect (apply (toEffect f) (toEffect a))

instance TypeEquals x y => Applicative (OmRender ctx x y) where
  pure a = fromEffect (Prelude.pure a)

instance TypeEquals x y => Bind (OmRender ctx x y) where
  bind m f = fromEffect (Prelude.bind (toEffect m) \a -> toEffect (f a))

instance TypeEquals x y => Monad (OmRender ctx x y)

instance (TypeEquals x y, Semigroup a) => Semigroup (OmRender ctx x y a) where
  append a b = fromEffect (append (toEffect a) (toEffect b))

instance (TypeEquals x y, Monoid a) => Monoid (OmRender ctx x y a) where
  mempty = fromEffect mempty

bind :: forall a b x y z ctx. OmRender ctx x y a -> (a -> OmRender ctx y z b) -> OmRender ctx x z b
bind = ibind

discard :: forall a b x y z ctx. OmRender ctx x y a -> (a -> OmRender ctx y z b) -> OmRender ctx x z b
discard = ibind

pure :: forall a x ctx. a -> OmRender ctx x x a
pure a = fromEffect (Prelude.pure a)

liftRender :: forall ctx x y a. Render x y a -> OmRender ctx x y a
liftRender = OmRender

data UseCtx :: Row Type -> Type -> Type
data UseCtx ctx hooks

useCtx :: forall @ctx hooks. OmRender ctx hooks (UseCtx ctx hooks) { | ctx }
useCtx = OmRender (unsafeHook readCtx)
  where
  readCtx :: Effect { | ctx }
  readCtx = unsafeCoerce (Ref.read ctxStash)

data UseOm :: Row Type -> Type -> Type -> Type
data UseOm ctx a hooks

useOm
  :: forall ctx deps a hooks
   . Eq deps
  => deps
  -> Om { | ctx } () a
  -> OmRender ctx hooks (UseOm ctx a hooks) (Maybe a)
useOm deps om = fromEffect Prelude.do
  ctx :: { | ctx } <- unsafeCoerce (Ref.read ctxStash)
  result /\ setResult <- toEffect (useState Nothing)
  toEffect $ useEffect deps Prelude.do
    setResult (const Nothing)
    fiber <- launchAff Prelude.do
      r <- try (Om.runOm ctx { exception: \e -> Aff.throwError e } om)
      liftEffect (setResult \_ -> Just r)
    Prelude.pure Prelude.do
      launchAff_ (killFiber (error "Stale request cancelled") fiber)
  toEffect $ liftRender $ unsafeRenderEffect case result of
    Just (Left err) -> Aff.throwError err
    Just (Right a) -> Prelude.pure (Just a)
    Nothing -> Prelude.pure Nothing

useState
  :: forall ctx state hooks
   . state
  -> OmRender ctx hooks (Hooks.UseState state hooks) (state /\ ((state -> state) -> Effect Unit))
useState s = liftRender (Hooks.useState s)

useState'
  :: forall ctx state hooks
   . state
  -> OmRender ctx hooks (Hooks.UseState state hooks) (state /\ (state -> Effect Unit))
useState' s = liftRender (Hooks.useState' s)

useEffect
  :: forall ctx deps hooks
   . Eq deps
  => deps
  -> Effect (Effect Unit)
  -> OmRender ctx hooks (Hooks.UseEffect deps hooks) Unit
useEffect deps effect = liftRender (Hooks.useEffect deps effect)

useEffectOnce
  :: forall ctx hooks
   . Effect (Effect Unit)
  -> OmRender ctx hooks (Hooks.UseEffect Unit hooks) Unit
useEffectOnce effect = liftRender (Hooks.useEffectOnce effect)

useEffectAlways
  :: forall ctx hooks
   . Effect (Effect Unit)
  -> OmRender ctx hooks (Hooks.UseEffect Unit hooks) Unit
useEffectAlways effect = liftRender (Hooks.useEffectAlways effect)

useLayoutEffect
  :: forall ctx deps hooks
   . Eq deps
  => deps
  -> Effect (Effect Unit)
  -> OmRender ctx hooks (Hooks.UseLayoutEffect deps hooks) Unit
useLayoutEffect deps effect = liftRender (Hooks.useLayoutEffect deps effect)

useLayoutEffectOnce
  :: forall ctx hooks
   . Effect (Effect Unit)
  -> OmRender ctx hooks (Hooks.UseLayoutEffect Unit hooks) Unit
useLayoutEffectOnce effect = liftRender (Hooks.useLayoutEffectOnce effect)

useLayoutEffectAlways
  :: forall ctx hooks
   . Effect (Effect Unit)
  -> OmRender ctx hooks (Hooks.UseLayoutEffect Unit hooks) Unit
useLayoutEffectAlways effect = liftRender (Hooks.useLayoutEffectAlways effect)

useRef
  :: forall ctx a hooks
   . a
  -> OmRender ctx hooks (Hooks.UseRef a hooks) (Hooks.Ref a)
useRef a = liftRender (Hooks.useRef a)

useMemo
  :: forall ctx deps a hooks
   . Eq deps
  => deps
  -> (Unit -> a)
  -> OmRender ctx hooks (Hooks.UseMemo deps a hooks) a
useMemo deps f = liftRender (Hooks.useMemo deps f)

useReducer
  :: forall ctx state action hooks
   . state
  -> Hooks.Reducer state action
  -> OmRender ctx hooks (Hooks.UseReducer state action hooks) (state /\ (action -> Effect Unit))
useReducer s r = liftRender (Hooks.useReducer s r)

useId :: forall ctx hooks. OmRender ctx hooks (Hooks.UseId hooks) String
useId = liftRender Hooks.useId

useTransition
  :: forall ctx hooks
   . OmRender ctx hooks (Hooks.UseTransition hooks) (Boolean /\ ((Effect Unit) -> Effect Unit))
useTransition = liftRender Hooks.useTransition

useDeferredValue
  :: forall ctx a hooks
   . a
  -> OmRender ctx hooks (Hooks.UseDeferredValue a hooks) a
useDeferredValue a = liftRender (Hooks.useDeferredValue a)

omComponent
  :: forall @ctx @err hooks props
   . String
  -> (props -> OmRender ctx Unit hooks JSX)
  -> Om { | ctx } err (props -> JSX)
omComponent name renderFn = Prelude.do
  ctx <- Om.ask
  let
    wrappedRender :: props -> Render Unit hooks JSX
    wrappedRender props =
      unsafeRenderEffect (Ref.write (unsafeCoerce ctx) ctxStash)
        `Render.discard` \_ -> un OmRender (renderFn props)
  Hooks.component name wrappedRender # liftEffect

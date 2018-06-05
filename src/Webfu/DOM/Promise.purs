module Webfu.DOM.Promise (
  Promise,
  mkPromise,
  mkResolve,
  mkReject,
  then_,
  then',
  catch_,
  finally_,
  race,
  all
) where

import Prelude (Unit)
import Effect (Effect)
import Data.Function.Uncurried (Fn1, runFn1, Fn2, runFn2)


foreign import data Promise :: Type -> Type -> Type


--------------------------------------------------------------------------------
-- mkPromise
--------------------------------------------------------------------------------

foreign import mkPromise_ffi
  :: forall a b
   . Fn1 ((a -> Effect Unit) -> (b -> Effect Unit) -> Effect Unit)
         (Effect (Promise a b))

-- | Creates a new Promise, accepting an *executor* function.
-- |
-- | The *executor* function accepts 2 functions as arguments:
-- |  - resolve: a function that resolves the promise with a success value.
-- |  - reject: a function that rejects the promise with an error value.
-- |
-- | The executor function normally performs some asynchronouse work, and is immediately executed by the
-- | Promise implementation.  If the async operation was successful, we use the `resolve` function to
-- | signal this, supplying a success value.  If the async operation failed, we use the `reject` function
-- | to signal failure, supplying a reason of what went wrong.
-- |
-- | ```purescript
-- | mkPromise (\ resolveF rejectF -> do
-- |            jsonValue <- fetchJsonValue "http://example.com/dummyJsonValue"
-- |            if jsonValue == ""
-- |              then rejectF "no value found"
-- |              else resolveF jsonValue )
-- | ```
mkPromise
  :: forall a b
   . ((a -> Effect Unit) -> (b -> Effect Unit) -> Effect Unit)
  -> Effect (Promise a b)
mkPromise = runFn1 mkPromise_ffi


--------------------------------------------------------------------------------
-- then_
--------------------------------------------------------------------------------

foreign import then_ffi
  :: forall a b
   . Fn2 (a -> Effect Unit)
         (Promise a b)
         (Effect (Promise a b))

then_ :: forall a b
       . (a -> Effect Unit)
      -> Promise a b
      -> Effect (Promise a b)
then_ = runFn2 then_ffi


--------------------------------------------------------------------------------
-- then'
--------------------------------------------------------------------------------

foreign import then2_ffi
  :: forall a b c d
   . Fn2 (a -> Effect (Promise c d))
         (Promise a b)
         (Effect (Promise c d))

then' :: forall a b c d
       . (a -> Effect (Promise c d))
      -> Promise a b
      -> Effect (Promise c d)
then' = runFn2 then2_ffi


--------------------------------------------------------------------------------
-- catch_
--------------------------------------------------------------------------------

foreign import catch_ffi
  :: forall a b
   . Fn2 (b -> Effect Unit)
         (Promise a b)
         (Effect (Promise a b))

catch_ :: forall a b eff
        . (b -> Effect Unit)
       -> Promise a b
       -> Effect (Promise a b)
catch_ = runFn2 catch_ffi



--------------------------------------------------------------------------------
-- finally_
--------------------------------------------------------------------------------

foreign import finally_ffi
  :: forall a b
   . Fn2 (Unit -> Effect Unit)
         (Promise a b)
         (Effect (Promise a b))

finally_ :: forall a b
        . (Unit -> Effect Unit)
       -> Promise a b
       -> Effect (Promise a b)
finally_ = runFn2 finally_ffi



--------------------------------------------------------------------------------
-- mkReject
--------------------------------------------------------------------------------

foreign import mkReject_ffi
  :: forall a b
   . Fn1 b
         (Effect (Promise a b))

mkReject :: forall a b
          . b
         -> Effect (Promise a b)
mkReject = runFn1 mkReject_ffi



--------------------------------------------------------------------------------
-- mkResolve
--------------------------------------------------------------------------------

foreign import mkResolve_ffi
  :: forall a b
   . Fn1 a
         (Effect (Promise a b))

mkResolve :: forall a b
           . a
          -> Effect (Promise a b)
mkResolve = runFn1 mkResolve_ffi



--------------------------------------------------------------------------------
-- race
--------------------------------------------------------------------------------

foreign import race_ffi
  :: forall a b
   . Fn1 (Array (Promise a b))
         (Effect (Promise a b))

race :: forall a b
      . Array (Promise a b)
     -> Effect (Promise a b)
race = runFn1 race_ffi



--------------------------------------------------------------------------------
-- all
--------------------------------------------------------------------------------

foreign import all_ffi
  :: forall a b
   . Fn1 (Array (Promise a b))
         (Effect (Promise a b))

all :: forall a b
     . Array (Promise a b)
    -> Effect (Promise a b)
all = runFn1 all_ffi

module DOM.Promise (
  Promise,
  mkPromise,
  mkResolve,
  mkReject,
  then_,
  catch_,
  finally_,
  race,
  all
) where

import Prelude
import Control.Monad.Eff
import Data.Function.Uncurried (Fn1, runFn1, Fn2, runFn2)
import DOM.Core (DOM)

foreign import data Promise :: Type -> Type -> Type


--------------------------------------------------------------------------------
-- mkPromise
--------------------------------------------------------------------------------

foreign import mkPromise_ffi
  :: forall eff a b
   . Fn1 ((a -> Eff eff Unit) -> (b -> Eff eff Unit) -> Eff eff Unit)
         (Eff (dom :: DOM | eff) (Promise a b))

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
  :: forall eff a b
   . ((a -> Eff eff Unit) -> (b -> Eff eff Unit) -> Eff eff Unit)
  -> Eff (dom :: DOM | eff) (Promise a b)
mkPromise = runFn1 mkPromise_ffi



--------------------------------------------------------------------------------
-- then_
--------------------------------------------------------------------------------

foreign import then_ffi
  :: forall a b eff
   . Fn2 (a -> Eff eff Unit)
         (Promise a b)
         (Eff (dom :: DOM | eff) (Promise a b))

then_ :: forall a b eff
       . (a -> Eff eff Unit)
      -> Promise a b
      -> Eff (dom :: DOM | eff) (Promise a b)
then_ = runFn2 then_ffi



--------------------------------------------------------------------------------
-- catch_
--------------------------------------------------------------------------------

foreign import catch_ffi
  :: forall a b eff
   . Fn2 (b -> Eff eff Unit)
         (Promise a b)
         (Eff (dom :: DOM | eff) (Promise a b))

catch_ :: forall a b eff
        . (b -> Eff eff Unit)
       -> Promise a b
       -> Eff (dom :: DOM | eff) (Promise a b)
catch_ = runFn2 catch_ffi



--------------------------------------------------------------------------------
-- finally_
--------------------------------------------------------------------------------

foreign import finally_ffi
  :: forall a b eff
   . Fn2 (Unit -> Eff eff Unit)
         (Promise a b)
         (Eff (dom :: DOM | eff) (Promise a b))

finally_ :: forall a b eff
        . (Unit -> Eff eff Unit)
       -> Promise a b
       -> Eff (dom :: DOM | eff) (Promise a b)
finally_ = runFn2 finally_ffi



--------------------------------------------------------------------------------
-- mkReject
--------------------------------------------------------------------------------

foreign import mkReject_ffi
  :: forall b eff
   . Fn1 b
         (Eff (dom :: DOM | eff) (Promise Unit b))

mkReject :: forall b eff
          . b
         -> Eff (dom :: DOM | eff) (Promise Unit b)
mkReject = runFn1 mkReject_ffi



--------------------------------------------------------------------------------
-- mkResolve
--------------------------------------------------------------------------------

foreign import mkResolve_ffi
  :: forall a eff
   . Fn1 a
         (Eff (dom :: DOM | eff) (Promise a Unit))

mkResolve :: forall a eff
           . a
          -> Eff (dom :: DOM | eff) (Promise a Unit)
mkResolve = runFn1 mkResolve_ffi



--------------------------------------------------------------------------------
-- race
--------------------------------------------------------------------------------

foreign import race_ffi
  :: forall a b eff
   . Fn1 (Array (Promise a b))
         (Eff (dom :: DOM | eff) (Promise a b))

race :: forall a b eff
      . Array (Promise a b)
     -> Eff (dom :: DOM | eff) (Promise a b)
race = runFn1 race_ffi



--------------------------------------------------------------------------------
-- all
--------------------------------------------------------------------------------

foreign import all_ffi
  :: forall a b eff
   . Fn1 (Array (Promise a b))
         (Eff (dom :: DOM | eff) (Promise a b))

all :: forall a b eff
     . Array (Promise a b)
    -> Eff (dom :: DOM | eff) (Promise a b)
all = runFn1 all_ffi


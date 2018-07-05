module Webfu.Interop
( JsObject
, toJsObject
, readBoolean
, readInt
, readNumber
, readString
, readChar
, runEffMethod0
) where


import Prelude (Unit, unit)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Data.Function.Uncurried


foreign import data JsObject :: Type

foreign import readProp :: forall a. Fn2 String JsObject a



-- | Convert a value into a JsObject.
-- | This function is unsafe, and it is the responsibility of the caller to make
-- | sure the underlying representation for both types are the same.
toJsObject :: forall a. a -> JsObject
toJsObject v = unsafeCoerce v

readBoolean :: String -> JsObject -> Boolean
readBoolean = runFn2 readProp

readInt :: String -> JsObject -> Int
readInt = runFn2 readProp

readNumber :: String -> JsObject -> Number
readNumber = runFn2 readProp

readString :: String -> JsObject -> String
readString = runFn2 readProp

readChar :: String -> JsObject -> Char
readChar = runFn2 readProp


foreign import runEffMethod0Impl :: Fn3 Unit String JsObject (Effect Unit)

runEffMethod0 :: String -> JsObject -> Effect Unit
runEffMethod0 = runFn3 runEffMethod0Impl unit

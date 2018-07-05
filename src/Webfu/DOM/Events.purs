module Webfu.DOM.Events
( Event
, MouseEvent
, type_
, bubbles
, cancelable
, defaultPrevented
, preventDefault
, detail
, screenX
, screenY
, clientX
, clientY
, button
, buttons
, ctrlKey
, shiftKey
, altKey
, metaKey
) where

import Prelude (Unit, unit, (<<<))
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Webfu.Interop
import Webfu.Data.Cast (class Cast)


foreign import data Event :: Type


type_ :: forall a. Cast a Event => a -> String
type_ = (readString "type") <<< toJsObject

bubbles :: forall a. Cast a Event => a -> String
bubbles = (readString "bubbles") <<< toJsObject

cancelable :: forall a. Cast a Event => a -> String
cancelable = (readString "cancelable") <<< toJsObject

defaultPrevented :: forall a. Cast a Event => a -> Boolean
defaultPrevented = (readBoolean "defaultPrevented") <<< toJsObject


foreign import preventDefaultImpl :: Fn2 Unit Event (Effect Unit)

preventDefault :: forall a. Cast a Event => a -> Effect Unit
preventDefault = (runEffMethod0 "preventDefault") <<< toJsObject



-----------------------------------------------------------
-- MouseEvent
-----------------------------------------------------------

foreign import data MouseEvent :: Type


instance castMouseEventToEvent :: Cast MouseEvent Event where
  cast = unsafeCoerce


detail :: MouseEvent -> Number
detail = (readNumber "detail") <<< toJsObject

screenX :: MouseEvent -> Int
screenX = (readInt "screenX") <<< toJsObject

screenY :: MouseEvent -> Int
screenY = (readInt "screenY") <<< toJsObject

clientX :: MouseEvent -> Int
clientX = (readInt "clientX") <<< toJsObject

clientY :: MouseEvent -> Int
clientY = (readInt "clientY") <<< toJsObject

button :: MouseEvent -> Int
button = (readInt "button") <<< toJsObject

buttons :: MouseEvent -> Int
buttons = (readInt "buttons") <<< toJsObject

ctrlKey :: MouseEvent -> Boolean
ctrlKey = (readBoolean "ctrlKey") <<< toJsObject

shiftKey :: MouseEvent -> Boolean
shiftKey = (readBoolean "shiftKey") <<< toJsObject

altKey :: MouseEvent -> Boolean
altKey = (readBoolean "altKey") <<< toJsObject

metaKey :: MouseEvent -> Boolean
metaKey = (readBoolean "metaKey") <<< toJsObject

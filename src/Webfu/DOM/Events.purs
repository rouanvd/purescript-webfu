module Webfu.DOM.Events where

import Prelude (Unit, unit)
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)

type Event r =
  { type             :: String
  , bubbles          :: Boolean
  , cancelable       :: Boolean
  , defaultPrevented :: Boolean
  | r
  }


foreign import preventDefaultImpl :: forall r. Fn2 Unit (Event r) (Effect Unit)

preventDefault :: forall r. Event r -> Effect Unit
preventDefault e = runFn2 preventDefaultImpl unit e


type MouseEvent =
  Event
    ( detail     :: Number
    , screenX    :: Int
    , screenY    :: Int
    , clientX    :: Int
    , clientY    :: Int
    , button     :: Int
    , buttons    :: Int
    , ctrlKey    :: Boolean
    , shiftKey   :: Boolean
    , altKey     :: Boolean
    , metaKey    :: Boolean
    )

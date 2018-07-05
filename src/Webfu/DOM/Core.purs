module Webfu.DOM.Core
  ( Node
  , Window
  , Document
  , Element
  , TypeError
  , typeError_message
  , typeError_name
  ) where

import Webfu.Data.Cast (class Cast)
import Data.Function.Uncurried (Fn1, runFn1)
import Unsafe.Coerce (unsafeCoerce)


-- https://dontcallmedom.github.io/webidlpedia/inheritance.html
-- https://developer.mozilla.org/en-US/docs/Web/API


foreign import data Window :: Type

foreign import data Node :: Type
foreign import data Document :: Type
foreign import data Element :: Type



instance convertable_Document_Node :: Cast Document Node where
  cast d = unsafeCoerce d

instance convertable_Element_Node :: Cast Element Node where
  cast e = unsafeCoerce e



--------------------------------------------------------------------------------
-- TypeError
--------------------------------------------------------------------------------
foreign import data TypeError :: Type

foreign import typeError_message_ffi :: TypeError -> String
typeError_message :: TypeError -> String
typeError_message te = runFn1 typeError_message_ffi te

foreign import typeError_name_ffi :: TypeError -> String
typeError_name :: TypeError -> String
typeError_name te = runFn1 typeError_name_ffi te

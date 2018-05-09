module Webfu.DOM.Core (
  class Convertible, convert,
  DOM,
  Node,
  Window,
  Document,
  Element
) where

import Control.Monad.Eff (kind Effect)
import Unsafe.Coerce (unsafeCoerce)


-- https://dontcallmedom.github.io/webidlpedia/inheritance.html
-- https://developer.mozilla.org/en-US/docs/Web/API


{-
  class Node

  class ParentNode

  type DocumentType
  (Node, ParentNode) => type Document
  (Node, ParentNode) => type Element
  type Attr

  (Node) => type Text
-}

foreign import data DOM :: Effect

foreign import data Window :: Type

foreign import data Node :: Type
foreign import data Document :: Type
foreign import data Element :: Type


class Convertible a b where
  convert :: a -> b

instance convertable_Document_Node :: Convertible Document Node where
  convert d = unsafeCoerce d

instance convertable_Element_Node :: Convertible Element Node where
  convert e = unsafeCoerce e


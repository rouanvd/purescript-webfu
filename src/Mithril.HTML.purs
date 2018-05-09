module Mithril.HTML where

import Data.Either
import Data.Array (foldl)
import Data.StrMap (StrMap)
import Data.StrMap (empty, insert, fold) as StrMap
import Data.ObjMap (Obj, empty, insert)
import Mithril (Component, VNode, mkTextVNode, mkVNode, mkComponentVNode)
import Prelude (class Show, show, (<>), (#))


---------------------------------------------------------------
-- ATTRIBUTE BUILDER
---------------------------------------------------------------

type Attributes = Obj
type AttributeSetterF = Attributes -> Attributes
type AttributeSpec = Array AttributeSetterF


insertObj :: forall a. String -> a -> AttributeSetterF
insertObj key val obj =
  insert obj key val


infixl 5 insertObj as :=


runAttrSpec :: AttributeSpec -> Attributes
runAttrSpec setterFs =
  foldl (\strMap setterF -> setterF strMap) empty setterFs


---------------------------------------------------------------
-- GENERIC STRING MAP BUILDER
---------------------------------------------------------------

type StrMapSetterF = StrMap String -> StrMap String
type StrMapSpec = Array StrMapSetterF

strMapInsert :: forall a. String -> String -> StrMapSetterF
strMapInsert = StrMap.insert

infixl 5 strMapInsert as :


---------------------------------------------------------------
-- CSS BUILDER
---------------------------------------------------------------

type CssRules = StrMap String
type CssRuleSetterF = CssRules -> CssRules
type CssRuleSpec = Array CssRuleSetterF


css :: CssRuleSpec -> String
css setterFs =
  foldl (\strMap setterF -> setterF strMap) StrMap.empty setterFs
  # StrMap.fold (\acc key val -> acc <> key <> ":" <> val <> ";") ""




---------------------------------------------------------------
-- MITHRIL COMPONENT
---------------------------------------------------------------

mcomp :: Component -> VNode
mcomp component = mkComponentVNode component


---------------------------------------------------------------
-- CONTENT
---------------------------------------------------------------

main :: AttributeSpec -> Array VNode -> VNode
main attrs childNodes = mkVNode (Left "main") (runAttrSpec attrs) childNodes

h1 :: AttributeSpec -> Array VNode -> VNode
h1 attrs childNodes = mkVNode (Left "h1") (runAttrSpec attrs) childNodes

h1' :: AttributeSpec -> String -> VNode
h1' attrs text = mkTextVNode (Left "h1") (runAttrSpec attrs) text

br :: AttributeSpec -> VNode
br attrs = mkVNode (Left "br") (runAttrSpec attrs) []

a :: AttributeSpec -> Array VNode -> VNode
a attrs childNodes = mkVNode (Left "a") (runAttrSpec attrs) childNodes

a' :: AttributeSpec -> String -> VNode
a' attrs text = mkTextVNode (Left "a") (runAttrSpec attrs) text

div :: AttributeSpec -> Array VNode -> VNode
div attrs childNodes = mkVNode (Left "div") (runAttrSpec attrs) childNodes



---------------------------------------------------------------
-- INPUT CONTROLS
---------------------------------------------------------------

button :: AttributeSpec -> Array VNode -> VNode
button attrs childNodes = mkVNode (Left "button") (runAttrSpec attrs) childNodes

button' :: AttributeSpec -> String -> VNode
button' attrs text = mkTextVNode (Left "button") (runAttrSpec attrs) text

select :: AttributeSpec -> Array VNode -> VNode
select attrs childNodes = mkVNode (Left "select") (runAttrSpec attrs) childNodes

option :: AttributeSpec -> String -> VNode
option attrs text = mkTextVNode (Left "option") (runAttrSpec attrs) text

input :: AttributeSpec -> VNode
input attrs = mkVNode (Left "input") (runAttrSpec attrs) []



---------------------------------------------------------------
-- SVG
---------------------------------------------------------------

svg :: AttributeSpec -> Array VNode -> VNode
svg attrs childNodes = mkVNode (Left "svg") (runAttrSpec (["xmlns" := "http://www.w3.org/2000/svg"] <> attrs)) childNodes

svgGroup :: AttributeSpec -> Array VNode -> VNode
svgGroup attrs childNodes = mkVNode (Left "g") (runAttrSpec attrs) childNodes

svgText :: AttributeSpec -> String -> VNode
svgText attrs text = mkTextVNode (Left "text") (runAttrSpec attrs) text

svgRect :: AttributeSpec -> VNode
svgRect attrs = mkVNode (Left "rect") (runAttrSpec attrs) []

svgCircle :: AttributeSpec -> VNode
svgCircle attrs = mkVNode (Left "circle") (runAttrSpec attrs) []

svgLine :: AttributeSpec -> VNode
svgLine attrs = mkVNode (Left "line") (runAttrSpec attrs) []

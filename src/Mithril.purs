module Mithril (
  VNode,
  Component,
  mkVNode,
  mkTextVNode,
  mkComponentVNode,
  mkComponent,
  onInit,
  onCreate,
  onBeforeUpdate,
  onUpdate,
  onBeforeRemove,
  onRemove,
  raise,
  mount,
  route,
  
  parseQueryString,
  buildQueryString,
  redraw,
  trust,
  version
) where

import Prelude (class Show, Unit, unit, ($))
import Data.Function.Uncurried (Fn1, runFn1, Fn3, runFn3, Fn4, runFn4, Fn5, runFn5, Fn6, runFn6)
import Data.Maybe
import Data.Either (Either(..), fromLeft, fromRight, isLeft)
import Data.StrMap (StrMap)
import Data.ObjMap (Obj)
import Control.Monad.Eff.Ref (REF, Ref, writeRef, readRef, newRef)
import DOM (DOM, Element)
import Partial.Unsafe (unsafePartial)
import Control.Monad.Eff (kind Effect, Eff)


type Attributes = Obj


---------------------------------------------------------------
-- VNODE
---------------------------------------------------------------

foreign import data VNode :: Type

foreign import vnodeTag :: VNode -> String

foreign import vnodeKey_foreign :: forall a. Maybe a -> (a -> Maybe a) -> VNode -> Maybe String
vnodeKey :: VNode -> Maybe String
vnodeKey vnode = vnodeKey_foreign Nothing Just vnode

foreign import vnodeChildren_foreign :: forall a. Maybe a -> (a -> Maybe a) -> VNode -> Maybe (Array VNode)
vnodeChildren :: VNode -> Maybe (Array VNode)
vnodeChildren vnode = vnodeChildren_foreign Nothing Just vnode


instance vnodeShow :: Show VNode where
  show vnode = "VNode"


foreign import mkVNode_foreign :: forall r. Fn6
                                  (Either String Element -> Boolean)
                                  (Either String Element -> String)
                                  (Either String Element -> Element)
                                  (Either String Element)
                                  Attributes
                                  (Either String (Array VNode))
                                  VNode

mkVNode :: forall r. (Either String Element) -> Attributes -> Array VNode -> VNode
mkVNode selector attrs childNodes = runFn6 (mkVNode_foreign) isLeft (unsafePartial $ fromLeft) (unsafePartial $ fromRight) selector attrs (Right childNodes)

mkTextVNode :: forall r. (Either String Element) -> Attributes -> String -> VNode
mkTextVNode selector attrs childNodes = runFn6 (mkVNode_foreign) isLeft (unsafePartial $ fromLeft) (unsafePartial $ fromRight) selector attrs (Left childNodes)


---------------------------------------------------------------
-- COMPONENT
---------------------------------------------------------------

foreign import data Component :: Type

foreign import mkComponent_foreign :: forall s eff. Fn5
                                      (s -> Eff (ref :: REF | eff) (Ref s))
                                      (Ref s -> Eff (ref :: REF | eff) s)
                                      (Ref s -> s -> Eff (ref :: REF | eff) Unit)
                                      s
                                      (Ref s -> s -> VNode -> VNode)
                                      Component

-- | Creates a new Component which uses the supplied view function.
mkComponent :: forall s. s -> (Ref s -> s -> VNode -> VNode) -> Component
mkComponent state viewF = runFn5 (mkComponent_foreign) (newRef) (readRef) (writeRef) state viewF


foreign import mkComponentVNode_foreign :: Fn1 Component VNode
mkComponentVNode :: Component -> VNode
mkComponentVNode component = runFn1 mkComponentVNode_foreign component


foreign import onInit_foreign :: forall s eff. Fn4 Unit (Ref s -> Eff (ref :: REF | eff) s) Component (Ref s -> s -> VNode -> Eff (dom :: DOM | eff) Unit) Component
onInit :: forall s eff. (Ref s -> s -> VNode -> Eff (dom :: DOM | eff) Unit) -> Component -> Component
onInit initF component = runFn4 onInit_foreign unit readRef component initF


foreign import onCreate_foreign :: forall s eff. Fn4 Unit (Ref s -> Eff (ref :: REF | eff) s) Component (Ref s -> s -> VNode -> Eff (dom :: DOM | eff) Unit) Component
onCreate :: forall s eff. (Ref s -> s -> VNode -> Eff (dom :: DOM | eff) Unit) -> Component -> Component
onCreate createF component = runFn4 onCreate_foreign unit readRef component createF


foreign import onBeforeUpdate_foreign :: forall s eff. Fn4 Unit (Ref s -> Eff (ref :: REF | eff) s) Component (Ref s -> s -> VNode -> Eff (dom :: DOM | eff) Unit) Component
onBeforeUpdate :: forall s eff. (Ref s -> s -> VNode -> Eff (dom :: DOM | eff) Unit) -> Component -> Component
onBeforeUpdate beforeUpdateF component = runFn4 onUpdate_foreign unit readRef component beforeUpdateF


foreign import onUpdate_foreign :: forall s eff. Fn4 Unit (Ref s -> Eff (ref :: REF | eff) s) Component (Ref s -> s -> VNode -> Eff (dom :: DOM | eff) Unit) Component
onUpdate :: forall s eff. (Ref s -> s -> VNode -> Eff (dom :: DOM | eff) Unit) -> Component -> Component
onUpdate updateF component = runFn4 onUpdate_foreign unit readRef component updateF


foreign import onBeforeRemove_foreign :: forall s eff. Fn4 Unit (Ref s -> Eff (ref :: REF | eff) s) Component (Ref s -> s -> VNode -> Eff (dom :: DOM | eff) Unit) Component
onBeforeRemove :: forall s eff. (Ref s -> s -> VNode -> Eff (dom :: DOM | eff) Unit) -> Component -> Component
onBeforeRemove beforeRemoveF component = runFn4 onBeforeRemove_foreign unit readRef component beforeRemoveF


foreign import onRemove_foreign :: forall s eff. Fn4 Unit (Ref s -> Eff (ref :: REF | eff) s) Component (Ref s -> s -> VNode -> Eff (dom :: DOM | eff) Unit) Component
onRemove :: forall s eff. (Ref s -> s -> VNode -> Eff (dom :: DOM | eff) Unit) -> Component -> Component
onRemove removeF component = runFn4 onRemove_foreign unit readRef component removeF


---------------------------------------------------------------
-- EVENTS
---------------------------------------------------------------

foreign import raise_foreign :: forall m s eff. Fn6
                                Unit
                                (Ref s -> Eff (ref :: REF | eff) s)
                                (Ref s -> s -> Eff (ref :: REF | eff) Unit)
                                (m -> s -> Eff eff s)
                                (Ref s)
                                m
                                Unit

raise :: forall m s eff. (m -> s -> Eff eff s) -> Ref s -> m -> Unit
raise updateF state msg = runFn6 (raise_foreign) unit readRef writeRef updateF state msg


---------------------------------------------------------------
-- MITHRIL CORE
---------------------------------------------------------------

foreign import render_foreign :: forall eff.
                                 Unit
                              -> (Either String (Array VNode) -> Boolean)
                              -> (Either String (Array VNode) -> String)
                              -> (Either String (Array VNode) -> Array VNode)
                              -> Element
                              -> Either String (Array VNode)
                              -> Eff (dom :: DOM | eff) Unit

-- | Renders a text node to the DOM
renderText :: forall eff. Element -> String -> Eff (dom :: DOM | eff) Unit
renderText elem vnodes = render_foreign unit isLeft (unsafePartial $ fromLeft) (unsafePartial $ fromRight) elem (Left vnodes)

-- | Renders 1 or more VNodes to the DOM
render :: forall eff. Element -> Array VNode -> Eff (dom :: DOM | eff) Unit
render elem vnodes = render_foreign unit isLeft (unsafePartial $ fromLeft) (unsafePartial $ fromRight) elem (Right vnodes)

foreign import mount_foreign :: forall eff. Fn3 Unit Element Component (Eff (dom :: DOM | eff) Unit)
mount :: forall eff. Element -> Component -> Eff (dom :: DOM | eff) Unit
mount elem component = runFn3 mount_foreign unit elem component

-- foreign import unmount :: forall eff. Element -> Eff (dom :: DOM | eff) Unit

foreign import route_foreign :: forall eff. Fn4 Unit Element String (StrMap Component) (Eff (dom :: DOM | eff) Unit)
-- | Configures the navigation for your Mithril application.
-- | You can only call `route` once per application.
route :: forall eff. Element -> String -> StrMap Component -> Eff (dom :: DOM | eff) Unit
route rootElem defaultRoute routes = runFn4 (route_foreign) unit rootElem defaultRoute routes


-- buildQueryString :: forall a. Generic a => a -> String
-- buildQueryString r =
--   let typRep = toSpine r in
--   case typRep of
--     SProd name [ctorF] ->
--       case ctorF unit of
--         SRecord fields -> foldl (\ acc f -> acc <> (if length acc > 0 then "," else "") <> f.recLabel <> "=" <> (show $ unsafePartial <<< fromJust <<< fromSpine $ f.recValue unit)) "" fields
--         _              -> "bluh"
--     _                 -> "bluh"
--     -- where
    --   recValueAsStr ::

-- | Turns a string of the form ?a=1&b=2 into an object
foreign import parseQueryString :: forall r. String -> {|r}

-- | Turns an object into a string of form a=1&b=2
foreign import buildQueryString :: forall r. {|r} -> String

foreign import redraw_foreign :: forall eff. Unit -> Eff (dom :: DOM | eff) Unit
-- | Updates the DOM after a change in the application data layer.
redraw :: forall eff. Eff (dom :: DOM | eff) Unit
redraw = redraw_foreign unit

-- | Turns an HTML string into unescaped HTML.
foreign import trust :: String -> VNode

-- | The semver version number of the current Mithril library.
foreign import version :: String

module App.DragDrop.Views.DragDropView
( mkView
) where

import Prelude (Unit, unit, pure, bind, discard, void, (>>=), ($), (<>), show)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref (read, write, modify) as Ref
import Effect.Unsafe
import Data.Array (find, findIndex, updateAt)
import Data.Number as Number
import Data.Maybe (Maybe(..), maybe)
import Webfu.Data.Err (Err(..))
import Webfu.DOM
import Webfu.DOM.Promise
import Webfu.Mithril (VNode, Component, mkComponent, raise, redraw)
import Webfu.Mithril.HTML
import App.Indicators.Views.DisplayScreenPresenterM (DisplayScreenPresenter)
import App.Indicators.Views.DisplayScreenPresenterM as Presenter
import App.Indicators.Views.Indicators (indRender)

-----------------------------------------------------------
-- STATE
-----------------------------------------------------------

type State =
  { pos1 :: Int
  , pos2 :: Int
  , pos3 :: Int
  , pos4 :: Int
  }


-----------------------------------------------------------
-- UPDATE
-----------------------------------------------------------
data Msg
  = OnMouseDown MouseEvent
  | OnMouseMove MouseEvent
  | OnMouseUp MouseEvent

update :: Ref State -> Msg -> Effect Unit
update refSt (OnMouseDown e) = do
  preventDefault e
  _ <- Ref.modify (\st -> st { pos3 = e.clientX, pos4 = e.clientY }) refSt
  maybeDiv1 <- doc_getElementById "div1"
  case maybeDiv1 of
    Nothing   -> pure unit
    Just div1 -> el_setOnMouseMove (\e -> pure $ raiseEvent refSt (OnMouseMove e)) div1
  -- log $ "mouse down: (" <> (show e.clientX) <> "," <> (show e.clientY) <> ") : " <> (show e.defaultPrevented)
  pure unit

update refSt (OnMouseMove e) = do
  log $ "mouse move: (" <> (show e.clientX) <> "," <> (show e.clientY) <> ") : " <> (show e.defaultPrevented)
  pure unit


update refSt (OnMouseUp e) = pure unit


-----------------------------------------------------------
-- VIEW
-----------------------------------------------------------

-- raiseEvent :: Ref Unit -> Msg -> Unit
-- raiseEvent refSt = raise update refSt

raiseEvent :: Ref State -> Msg -> Unit
raiseEvent refSt = raise update refSt


mkView :: Unit -> Component
mkView _ =
  let initialState = { pos1:0, pos2:0, pos3:0, pos4:0 } in
  mkComponent initialState (\refSt _ -> do
    st <- Ref.read refSt
    pure $ div ["id":="div1"
               ,"style":=css["position":"absolute", "background-color":"black", "width":"100px", "height":"100px"]
               , onMouseDown \e -> pure $ raiseEvent refSt (OnMouseDown e)
               -- , onMouseMove \e -> pure $ raiseEvent refSt (OnMouseMove e)
               -- , onMouseUp \e -> pure $ raiseEvent refSt (OnMouseUp e)
               ] [

      ])

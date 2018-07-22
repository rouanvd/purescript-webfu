module App.Indicators.Views.DisplayScreenView (
  mkView
) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref (read, write) as Ref
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


-----------------------------------------------------------
-- UPDATE
-----------------------------------------------------------
data Msg
  = LoadIndicators
  | IncrementValues
  | IncreaseZoom
  | DecreaseZoom

update :: Ref DisplayScreenPresenter -> Msg -> Effect Unit
update refPr LoadIndicators = void $
  Presenter.loadIndicators refPr
  >>= (thn' (\_ -> Presenter.loadValues refPr))
  >>= (thn_ (\_ -> redraw))
  >>= (catch_ (\(Err err) -> log err))

update refPr IncrementValues = Presenter.incrementIndicatorValues refPr
update refPr IncreaseZoom = Presenter.increaseZoom refPr
update refPr DecreaseZoom = Presenter.decreaseZoom refPr


-----------------------------------------------------------
-- VIEW
-----------------------------------------------------------

raiseEvent :: Ref DisplayScreenPresenter -> Msg -> Unit
raiseEvent refSt = raise update refSt


-- mkView :: forall eff. Unit -> Eff eff Component
mkView :: DisplayScreenPresenter -> Component
mkView presenter =
  mkComponent presenter (\refPr _ -> do
    p <- Ref.read refPr
    pure $ main [] [
      h1' ["style":="color:red;"] "Bukela",
      --mcomp (HWComponent.mkView { name: "RvD" }),
      button' ["onclick":= \_ -> raiseEvent refPr IncrementValues] "++",
      button' ["onclick":= \_ -> raiseEvent refPr LoadIndicators] "??",
      -- select ["id":="indicatorName"] (map (\ind -> option ["value":= indId ind] (indId ind)) st.indicators),
      -- input ["type":= "text", "id":= "newIndicatorValue", "style":="width:450px"],
      -- button' ["onclick":= \_ -> raiseEvent stRef SetIndicatorValue] "!!",
      button' ["onclick":= \_ -> raiseEvent refPr IncreaseZoom] "Z+",
      button' ["onclick":= \_ -> raiseEvent refPr DecreaseZoom] "Z-",
      --br [],
      svg ["id":="svg", "viewBox":="0 0 300 150"] (map indRender p.model.indicators)
      ])

module Views.Bukela (
  state,
  mkView
) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (Ref, newRef)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Array (find, findIndex, updateAt)
import Data.Number as Number
import Data.Maybe (Maybe(..), maybe)
import Webfu.DOM
import Webfu.Mithril (VNode, Component, mkComponent, raise)
import Webfu.Mithril.HTML

import Views.Bukela.Controls
import Views.HelloWorldComponent as HWComponent

-----------------------------------------------------------
-- STATE
-----------------------------------------------------------

type State = {indicators :: Array IndicatorCtl}

state :: State
state = {indicators: getIndicators unit}


-----------------------------------------------------------
-- UPDATE
-----------------------------------------------------------
data Msg = IncrementValues
         | AlertDomValues
         | SetIndicatorValue
         | IncreaseZoom
         | DecreaseZoom

update :: forall eff. Msg -> State -> Eff (dom :: DOM |eff) State
update IncrementValues st = do
  let updatedIndicators = map (indIncValues 1.0) st.indicators
  pure $ st { indicators = updatedIndicators }

update AlertDomValues st = do
  maybeFirstIndicator <- doc_querySelector $ "#" <> "C02Buffer" <> " .innerBox"
  let msg = maybe (Just "ERROR: could not find first indicator.") (elem_attr "height") maybeFirstIndicator
  window >>= win_alert (maybe "blah" id msg)
  pure st

update SetIndicatorValue st = do
  maybeIndName  <- doc_getElementById "indicatorName" >>= \elem -> pure $ maybe Nothing (el_prop_value) elem
  maybeNewValue <- doc_getElementById "newIndicatorValue" >>= \elem -> pure $ (maybe Nothing (Number.fromString) (maybe Nothing (el_prop_value) elem))
  let maybeIndicator = maybeIndName >>= findIndicator st

  let maybeUpdatedInd :: Maybe IndicatorCtl
      maybeUpdatedInd = updateIndicator <$> maybeIndicator <*> maybeNewValue

  pure $ (maybe st (\ind -> st { indicators = updateIndicatorIn st.indicators ind }) maybeUpdatedInd)
  where
    updateIndicator :: IndicatorCtl -> Number -> IndicatorCtl
    updateIndicator ind newVal = indSetValues [newVal] ind

    updateIndicatorIn :: Array IndicatorCtl -> IndicatorCtl -> Array IndicatorCtl
    updateIndicatorIn arr ind =
      let maybeUpdatedArr = (findIndex (\ i -> (indId i) == (indId ind)) arr) >>= (\idx -> updateAt idx ind arr)
      in maybe arr id maybeUpdatedArr

update IncreaseZoom st = do
  pure $ st { indicators = map (indIncScale 0.1) st.indicators }

update DecreaseZoom st = do
  pure $ st { indicators = map (indDecScale 0.1) st.indicators }



findIndicator :: State -> String -> Maybe IndicatorCtl
findIndicator st name = st.indicators # find (\ind -> (indId ind) == name)

-----------------------------------------------------------
-- VIEW
-----------------------------------------------------------

raiseEvent :: Ref State -> Msg -> Unit
raiseEvent refSt = raise update refSt


-- mkView :: forall eff. Unit -> Eff eff Component
mkView :: State -> Component
mkView state =
  mkComponent state (\stRef st _ ->
    main [] [
      h1' ["style":="color:red;"] "Bukela",
      mcomp (HWComponent.mkView { name: "RvD" }),
      button' ["onclick":= \_ -> raiseEvent stRef IncrementValues] "++",
      button' ["onclick":= \_ -> raiseEvent stRef AlertDomValues] "??",
      select ["id":="indicatorName"] (map (\ind -> option ["value":= indId ind] (indId ind)) st.indicators),
      input ["type":= "text", "id":= "newIndicatorValue", "style":="width:450px"],
      button' ["onclick":= \_ -> raiseEvent stRef SetIndicatorValue] "!!",
      button' ["onclick":= \_ -> raiseEvent stRef IncreaseZoom] "Z+",
      button' ["onclick":= \_ -> raiseEvent stRef DecreaseZoom] "Z-",
      br [],
      svg ["id":="svg", "viewBox":="0 0 300 150"] (map indRender st.indicators)
      ])


-- mkIndicator :: IndicatorCtl -> VNode
-- mkIndicator (Indicator ind) =
--   let
--     outerBox     = svgRect ["id":= (ind.name <> "2"), "x":= ind.x, "y":= ind.y, "width":= 20, "height":= ind.maxValue + 2, "class":= "outerBox"]
--     innerBox     = svgRect ["x":= ind.x + 1, "y":= ind.y + (ind.maxValue - ind.value) + 1, "width":= 18, "height":= ind.value, "class":= "innerBox"]
--     label        = svgText ["x":= ind.x, "y":= ind.y - 4, "class":= "indicator-label"] ind.desc
--     valueBox     = svgRect ["x":= ind.x + 20 + 2, "y":= ind.y + (ind.maxValue / 2) - 5, "width":= 10, "height":= 10, "class":= "indicator-value-box"]
--     valueBoxText = svgText ["x" := ind.x + 20 + 3, "y" := ind.y + (ind.maxValue / 2) + 2, "class" := "indicator-value"] (show ind.value)
--   in
--     svgGroup ["id":= ind.name, "transform":=("scale(" <> (show ind.scale) <> ")")] [
--       label,
--       outerBox,
--       innerBox,
--       valueBox,
--       valueBoxText
--       ]

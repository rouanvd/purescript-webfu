module App.Indicators.Views.DisplayScreenPresenterM
( DisplayScreenPresenter
, State
, mkDisplayScreenPresenter
, loadIndicators
, loadValues
, incrementIndicatorValues
, increaseZoom
, decreaseZoom
) where

import Prelude
import Data.Either
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Webfu.DOM (window)
import Webfu.DOM.Fetch (win_fetch, responseBodyAsJson, responseOk)
import Webfu.DOM.Promise (Promise, thn_, thn', catch_, mkReject, mkResolve)
import Webfu.Data.Err (Err(..))
import App.Indicators.Models (BooleanIndProperties, GuageIndProperties, Indicator(..), IndValue, LabelIndProperties, Orientation(..))
import App.Indicators.Models as Model
import App.Indicators.Models.Json (readIndicatorsOrError, readIndicatorValuesOrError)


-----------------------------------------------------------
-- STATE
-----------------------------------------------------------

type State = {indicators :: Array Indicator}


type DisplayScreenPresenter =
  { model :: State
  }


mkDisplayScreenPresenter :: State -> DisplayScreenPresenter
mkDisplayScreenPresenter initState =
  { model: initState }


loadIndicators :: Ref DisplayScreenPresenter -> Effect (Promise Unit Err)
loadIndicators refPr = do
  w <- window
  p <- (win_fetch "http://localhost:8080/indicators.json" w)
       >>= (thn' (\rqst -> if responseOk rqst
                             then pure $ responseBodyAsJson rqst
                             else mkReject (Err "bla bla")))
       >>= (thn' (\json -> case readIndicatorsOrError json of
                             Left err         -> mkReject (Err err)
                             Right indicators -> do Ref.write { model: { indicators: indicators }} refPr
                                                    mkResolve unit))
       -- >>= (thn_ (\json -> Ref.write { model: { indicators: readIndicatorsAlwaysValid json }} refPr))
  pure p


loadValues :: Ref DisplayScreenPresenter -> Effect (Promise Unit Err)
loadValues refPr = do
  w <- window
  p <- (win_fetch "http://localhost:8080/values.json" w)
       >>= (thn' (\rqst -> if responseOk rqst
                             then pure $ responseBodyAsJson rqst
                             else mkReject (Err "bla bla")))
       >>= (thn' (\json -> case readIndicatorValuesOrError json of
                             Left err     -> mkReject (Err err)
                             Right values -> do updateIndicatorsWithValues refPr values
                                                mkResolve unit))
       -- >>= (thn_ (\json -> Ref.write { model: { indicators: readIndicatorsAlwaysValid json }} refPr))
  pure p


updateIndicatorsWithValues :: Ref DisplayScreenPresenter -> Array IndValue -> Effect Unit
updateIndicatorsWithValues refPr values = do
  presenter <- Ref.read refPr
  let updatedIndicators = presenter.model.indicators # map \ind -> Model.indSetValues (Model.lookupValuesForIndicator values ind) ind
  Ref.write (presenter { model { indicators = updatedIndicators } }) refPr



incrementIndicatorValues :: Ref DisplayScreenPresenter -> Effect Unit
incrementIndicatorValues refPr =
  Ref.modify_ (\pr -> pr { model { indicators = map (Model.indIncValues 1.0) pr.model.indicators }}) refPr


increaseZoom :: Ref DisplayScreenPresenter -> Effect Unit
increaseZoom refPr =
  Ref.modify_ (\pr -> pr { model { indicators = map (Model.indIncScale 0.1) pr.model.indicators }}) refPr


decreaseZoom :: Ref DisplayScreenPresenter -> Effect Unit
decreaseZoom refPr =
  Ref.modify_ (\pr -> pr { model { indicators = map (Model.indDecScale 0.1) pr.model.indicators }}) refPr


-- readIndicatorsAlwaysValid :: Foreign -> Array Indicator
-- readIndicatorsAlwaysValid v =
--   unsafePartial $ fromRight $ readIndicators v

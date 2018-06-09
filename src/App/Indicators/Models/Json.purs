module App.Indicators.Models.Json
( readIndicatorsOrError
, readIndicatorValuesOrError
) where

import Prelude (($), bind, (>>=), (=<<), show, pure, (<>))
import Data.Either (Either(..))
import Data.NonEmpty (head)
import Data.List.Types (NonEmptyList(..))
import Data.Traversable (traverse)
import Control.Monad.Except (runExcept)
import Foreign (Foreign, F, ForeignError(..), readInt, readNumber, readString, readArray, fail)
import Foreign.Index ((!))
import App.Indicators.Models


-----------------------------------------------------------
-- INDICATORS
-----------------------------------------------------------

readIndicatorsOrError :: Foreign -> Either String (Array Indicator)
readIndicatorsOrError fVal =
  let readResult = readIndicators fVal in
  case readResult of
    Left (NonEmptyList errs) -> Left $ show (head errs)
    Right val -> Right val


readIndicators :: Foreign -> Either (NonEmptyList ForeignError) (Array Indicator)
readIndicators fVal =
  runExcept $ traverse readIndicator =<< readArray fVal


readIndicator :: Foreign -> F Indicator
readIndicator v = do
  indType <- v ! "indType" >>= readString
  case indType of
    "label"   -> readLabelIndicator v
    "boolean" -> readBooleanIndicator v
    "guage"   -> readGuageIndicator v
    _         -> readUnknownIndicator v  -- fail $ ForeignError ("indType: " <> indType <> " is unhandled!")


readOrientation :: Foreign -> F Orientation
readOrientation v = do
  orientation <- readString v
  case orientation of
    "p" -> pure Portrait
    "l" -> pure Landscape
    _ -> fail $ ForeignError ("orientation: '" <> orientation <> "' is unhandled!")


readLabelIndicator :: Foreign -> F Indicator
readLabelIndicator v = do
  indType      <- v ! "indType" >>= readString
  id           <- v ! "id" >>= readString
  x            <- v ! "x" >>= readInt
  y            <- v ! "y" >>= readInt
  z            <- v ! "z" >>= readInt
  width        <- v ! "width" >>= readInt
  height       <- v ! "height" >>= readInt
  rotation     <- v ! "rotation" >>= readNumber
  scale        <- v ! "scale" >>= readNumber
  orientation  <- v ! "orientation" >>= readOrientation
  text         <- v ! "text" >>= readString
  values       <- v ! "values" >>= readArray >>= (traverse readNumber)

  fontSizeInPx <- v ! "fontSizeInPx" >>= readNumber

  pure $ LabelInd
    { indType
    , id
    , x
    , y
    , z
    , width
    , height
    , rotation
    , scale
    , orientation
    , text
    , values
    , fontSizeInPx
    }


readBooleanIndicator :: Foreign -> F Indicator
readBooleanIndicator v = do
  indType      <- v ! "indType" >>= readString
  id           <- v ! "id" >>= readString
  x            <- v ! "x" >>= readInt
  y            <- v ! "y" >>= readInt
  z            <- v ! "z" >>= readInt
  width        <- v ! "width" >>= readInt
  height       <- v ! "height" >>= readInt
  rotation     <- v ! "rotation" >>= readNumber
  scale        <- v ! "scale" >>= readNumber
  orientation  <- v ! "orientation" >>= readOrientation
  text         <- v ! "text" >>= readString
  values       <- v ! "values" >>= readArray >>= (traverse readNumber)

  onValue      <- v ! "onValue" >>= readString
  onColor      <- v ! "onColor" >>= readString
  offValue     <- v ! "offValue" >>= readString
  offColor     <- v ! "offColor" >>= readString

  pure $ BooleanInd
    { indType
    , id
    , x
    , y
    , z
    , width
    , height
    , rotation
    , scale
    , orientation
    , text
    , values
    , onValue
    , onColor
    , offValue
    , offColor
    }


readGuageIndicator :: Foreign -> F Indicator
readGuageIndicator v = do
  indType      <- v ! "indType" >>= readString
  id           <- v ! "id" >>= readString
  x            <- v ! "x" >>= readInt
  y            <- v ! "y" >>= readInt
  z            <- v ! "z" >>= readInt
  width        <- v ! "width" >>= readInt
  height       <- v ! "height" >>= readInt
  rotation     <- v ! "rotation" >>= readNumber
  scale        <- v ! "scale" >>= readNumber
  orientation  <- v ! "orientation" >>= readOrientation
  text         <- v ! "text" >>= readString
  values       <- v ! "values" >>= readArray >>= (traverse readNumber)

  maxValue     <- v ! "maxValue" >>= readNumber

  pure $ GuageInd
    { indType
    , id
    , x
    , y
    , z
    , width
    , height
    , rotation
    , scale
    , orientation
    , text
    , values
    , maxValue
    }


readUnknownIndicator :: Foreign -> F Indicator
readUnknownIndicator v = do
  indType      <- v ! "indType" >>= readString
  id           <- v ! "id" >>= readString
  x            <- v ! "x" >>= readInt
  y            <- v ! "y" >>= readInt
  z            <- v ! "z" >>= readInt
  width        <- v ! "width" >>= readInt
  height       <- v ! "height" >>= readInt
  rotation     <- v ! "rotation" >>= readNumber
  scale        <- v ! "scale" >>= readNumber
  orientation  <- v ! "orientation" >>= readOrientation
  text         <- v ! "text" >>= readString
  values       <- v ! "values" >>= readArray >>= (traverse readNumber)

  pure $ UnknownInd
    { indType
    , id
    , x
    , y
    , z
    , width
    , height
    , rotation
    , scale
    , orientation
    , text
    , values
    }


-----------------------------------------------------------
-- INDICATOR VALUES
-----------------------------------------------------------

readIndicatorValuesOrError :: Foreign -> Either String (Array IndValue)
readIndicatorValuesOrError fVal =
  let readResult = readIndicatorValues fVal in
  case readResult of
    Left (NonEmptyList errs) -> Left $ show (head errs)
    Right val -> Right val


readIndicatorValues :: Foreign -> Either (NonEmptyList ForeignError) (Array IndValue)
readIndicatorValues fVal =
  runExcept $ traverse readIndValue =<< readArray fVal


readIndValue :: Foreign -> F IndValue
readIndValue v = do
  indId <- v ! "indId" >>= readString
  value <- v ! "value" >>= readString
  tagId <- v ! "tagId" >>= readString

  pure { indId, value, tagId }

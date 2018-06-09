module App.Indicators.Models
( IndValue
, Orientation(..)
, ColorRange
, IndProperties
, BooleanIndProperties
, LabelIndProperties
, GuageIndProperties
, Indicator(..)
, lookupValuesForIndicator
, indText'
, indText
, indSetText
, indSetTextFromValues
, indValue'
, indValue
, indIntValue'
, indIntValue
, indIncValues
, indSetValues
, indIncScale
, indDecScale
, blnGetColor
, ggeIntMaxValue
) where

import Prelude
import Data.Foldable (foldl)
import Data.Int (toNumber, round)
import Data.Array (length, filter, intercalate)


-----------------------------------------------------------
-- INDICATOR VALUE DATA & FUNCTIONS
-----------------------------------------------------------

type IndValue =
  { indId :: String
  , value :: String
  , tagId :: String
  }


lookupValuesForIndicator :: Array IndValue -> Indicator -> Array String
lookupValuesForIndicator values ind =
  values # filter (\v -> v.indId == indId ind)
         # map _.value


-----------------------------------------------------------
-- BASE INDICATOR DATA & FUNCTIONS
-----------------------------------------------------------

data Orientation
  = Portrait
  | Landscape


data ColorRange


type IndProperties r =
  { indType     :: String
  , id          :: String
  , x           :: Int
  , y           :: Int
  , z           :: Int
  , width       :: Int
  , height      :: Int
  , rotation    :: Number
  , scale       :: Number
  , orientation :: Orientation
  , text        :: String
  , values      :: Array Number
  | r
  }


indSetScale' :: forall r. Number -> IndProperties r -> IndProperties r
indSetScale' scale props = props { scale = scale }

indIncScale' :: forall r. Number -> IndProperties r -> IndProperties r
indIncScale' inc props = props { scale = props.scale + inc }

indDecScale' :: forall r. Number -> IndProperties r -> IndProperties r
indDecScale' dec props = props { scale = props.scale - dec }

indText' :: forall r. IndProperties r -> String
indText' props = props.text

indSetText' :: forall r. String -> IndProperties r -> IndProperties r
indSetText' text props = props { text = text }

indValue' :: forall r. IndProperties r -> Number
indValue' props =
  if length props.values <= 0
    then 0.0
    else props.values # foldl (+) 0.0 # (_ / (toNumber $ length props.values))

indIntValue' :: forall r. IndProperties r -> Int
indIntValue' props =
  if length props.values <= 0
    then 0
    else props.values # foldl (+) 0.0 # (_ / (toNumber $ length props.values)) # round

indIncValues' :: forall r. Number -> IndProperties r -> IndProperties r
indIncValues' inc props =
  if length props.values <= 0
    then props { values = [inc] }
    else props { values = map (_ + inc) props.values }

indDecValues' :: forall r. Number -> IndProperties r -> IndProperties r
indDecValues' dec props =
  if length props.values <= 0
    then props { values = [0.0 - dec] }
    else props { values = map (_ - dec) props.values }




data Indicator
  = LabelInd LabelIndProperties
  | BooleanInd BooleanIndProperties
  | GuageInd GuageIndProperties
  | UnknownInd (IndProperties ())


indMap :: (forall r. IndProperties r -> IndProperties r) -> Indicator -> Indicator
indMap f (LabelInd p)   = LabelInd $ f p
indMap f (BooleanInd p) = BooleanInd $ f p
indMap f (GuageInd p)   = GuageInd $ f p
indMap f (UnknownInd p) = UnknownInd $ f p

indGet :: forall a. (forall r. IndProperties r -> a) -> Indicator -> a
indGet f (LabelInd p)   = f p
indGet f (BooleanInd p) = f p
indGet f (GuageInd p)   = f p
indGet f (UnknownInd p) = f p

indId :: Indicator -> String
indId (LabelInd {id})   = id
indId (BooleanInd {id}) = id
indId (GuageInd {id})   = id
indId (UnknownInd {id}) = id

indSetScale :: Number -> Indicator -> Indicator
indSetScale scale = indMap (indSetScale' scale)

indIncScale :: Number -> Indicator -> Indicator
indIncScale inc = indMap (indIncScale' inc)

indDecScale :: Number -> Indicator -> Indicator
indDecScale dec = indMap (indDecScale' dec)

indText :: Indicator -> String
indText = indGet indText'

indSetText :: String -> Indicator -> Indicator
indSetText text = indMap (indSetText' text)

indSetTextFromValues :: Array String -> Indicator -> Indicator
indSetTextFromValues values = indSetText $ intercalate ", " values

indValue :: Indicator -> Number
indValue = indGet indValue'

indIntValue :: Indicator -> Int
indIntValue = indGet indIntValue'

indValues :: Indicator -> Array Number
indValues = indGet _.values

indSetValues :: Array Number -> Indicator -> Indicator
indSetValues vs = indMap _ { values = vs }

indIncValues :: Number -> Indicator -> Indicator
indIncValues inc = indMap (indIncValues' inc)

indDecValues :: Number -> Indicator -> Indicator
indDecValues dec = indMap (indDecValues' dec)




-----------------------------------------------------------
-- LABEL CONTROL
-----------------------------------------------------------

type LabelIndProperties =
  IndProperties
  ( fontSizeInPx :: Number
  )


-----------------------------------------------------------
-- BOOLEAN CONTROL
-----------------------------------------------------------

type BooleanIndProperties =
  IndProperties
  ( onValue  :: String
  , onColor  :: String
  , offValue :: String
  , offColor :: String
  )


blnGetColor :: BooleanIndProperties -> String
blnGetColor props =
  let
    v :: Number
    v = indValue' props
  in
    if (show v) == props.onValue
      then props.onColor
      else props.offColor


-----------------------------------------------------------
-- GUAGE CONTROL
-----------------------------------------------------------

type GuageIndProperties =
  IndProperties
  ( maxValue :: Number
  )


ggeIntMaxValue :: GuageIndProperties -> Int
ggeIntMaxValue props = round props.maxValue

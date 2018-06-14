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
, indIncValues
, indSetValues
, indIncScale
, indDecScale
, blnGetColor'
, ggeValue'
, ggeIntValue'
, ggeIntMinValue'
, ggeIntMaxValue'
) where

import Prelude
import Data.Maybe
import Data.Foldable (foldl)
import Data.Int (toNumber, round)
import Data.Number as Number
import Data.Array as Array


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
  values # Array.filter (\v -> v.indId == indId ind)
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
indSetTextFromValues values = indSetText $ Array.intercalate ", " values

indSetValues :: Array String -> Indicator -> Indicator
indSetValues vs (LabelInd props)   = LabelInd $ lblSetValues' vs props
indSetValues vs (BooleanInd props) = BooleanInd $ blnSetValues' vs props
indSetValues vs (GuageInd props)   = GuageInd $ ggeSetValues' vs props
indSetValues vs ind@(UnknownInd _) = ind

indIncValues :: Number -> Indicator -> Indicator
indIncValues n ind@(LabelInd props) = ind
indIncValues n (BooleanInd props)   = BooleanInd $ blnOn' props
indIncValues n (GuageInd props)     = GuageInd $ ggeIncValues' n props
indIncValues n ind@(UnknownInd _)   = ind


indDecValues :: Number -> Indicator -> Indicator
indDecValues n ind@(LabelInd props) = ind
indDecValues n (BooleanInd props)   = BooleanInd $ blnOff' props
indDecValues n (GuageInd props)     = GuageInd $ ggeDecValues' n props
indDecValues n ind@(UnknownInd _)   = ind




-----------------------------------------------------------
-- LABEL CONTROL
-----------------------------------------------------------

type LabelIndProperties =
  IndProperties
  ( values       :: Array String
  , fontSizeInPx :: Number
  )


lblSetValues' :: Array String -> LabelIndProperties -> LabelIndProperties
lblSetValues' vs props = props { values = vs }



-----------------------------------------------------------
-- BOOLEAN CONTROL
-----------------------------------------------------------

type BooleanIndProperties =
  IndProperties
  ( values   :: Array Boolean
  , onColor  :: String
  , offColor :: String
  )


blnSetValues' :: Array String -> BooleanIndProperties -> BooleanIndProperties
blnSetValues' vs props =
  props { values = Array.mapMaybe toBool vs }
  where
    toBool :: String -> Maybe Boolean
    toBool "True"  = Just true
    toBool "False" = Just false
    toBool _       = Nothing


blnValue' :: BooleanIndProperties -> Boolean
blnValue' props =
  Array.foldl (||) false props.values

blnOn' ::BooleanIndProperties -> BooleanIndProperties
blnOn' props = props { values = [true] }


blnOff' ::BooleanIndProperties -> BooleanIndProperties
blnOff' props = props { values = [false] }


blnGetColor' :: BooleanIndProperties -> String
blnGetColor' props =
  let
    v :: Boolean
    v = blnValue' props
  in
    if v
      then props.onColor
      else props.offColor


-----------------------------------------------------------
-- GUAGE CONTROL
-----------------------------------------------------------

type GuageIndProperties =
  IndProperties
  ( values   :: Array Number
  , minValue :: Number
  , maxValue :: Number
  )


ggeSetValues' :: Array String -> GuageIndProperties -> GuageIndProperties
ggeSetValues' vs props =
  props { values = Array.mapMaybe Number.fromString vs }


ggeValue' :: GuageIndProperties -> Number
ggeValue' props =
  if Array.length props.values <= 0
    then 0.0
    else props.values # foldl (+) 0.0 # (_ / (toNumber $ Array.length props.values))


ggeIntValue' :: GuageIndProperties -> Int
ggeIntValue' props = round $ ggeValue' props


ggeIncValues' :: Number -> GuageIndProperties -> GuageIndProperties
ggeIncValues' inc props =
  if Array.length props.values <= 0
    then props { values = [inc] }
    else props { values = map (_ + inc) props.values }


ggeDecValues' :: Number -> GuageIndProperties -> GuageIndProperties
ggeDecValues' dec props =
  if Array.length props.values <= 0
    then props { values = [0.0 - dec] }
    else props { values = map (_ - dec) props.values }


ggeIntMinValue' :: GuageIndProperties -> Int
ggeIntMinValue' props = round props.minValue


ggeIntMaxValue' :: GuageIndProperties -> Int
ggeIntMaxValue' props = round props.maxValue

module Views.Bukela.Controls
( Orientation
, ColorRange
, CtlProperties
, LabelCtlProperties
, BooleanCtlProperties
, GuageCtlProperties
, IndicatorCtl(..)
, indId
, indSetScale
, indIncScale
, indDecScale
, indValues
, indSetValues
, indIncValues
, indDecValues
, indRender
, getIndicators
) where

import Prelude
import Data.Array (length)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Webfu.Mithril (VNode, Component, mkComponent, raise)
import Webfu.Mithril.HTML

-----------------------------------------------------------
-- GENERAL CONTROL DATA & FUNCTIONS
-----------------------------------------------------------

data Orientation
  = Portrait
  | Landscape


data ColorRange


type CtlProperties r =
  { id          :: String
  , x           :: Int
  , y           :: Int
  , width       :: Int
  , height      :: Int
  , rotation    :: Number
  , scale       :: Number
  , orientation :: Orientation
  , values      :: Array Number
  | r
  }


ctlSetScale :: forall r. Number -> CtlProperties r -> CtlProperties r
ctlSetScale scale props = props { scale = scale }

ctlIncScale :: forall r. Number -> CtlProperties r -> CtlProperties r
ctlIncScale inc props = props { scale = props.scale + inc }

ctlDecScale :: forall r. Number -> CtlProperties r -> CtlProperties r
ctlDecScale dec props = props { scale = props.scale - dec }

ctlValue :: forall r. CtlProperties r -> Number
ctlValue props = props.values # foldl (+) 0.0 # (_ / (toNumber $ length props.values))

ctlIncValues :: forall r. Number -> CtlProperties r -> CtlProperties r
ctlIncValues inc props = props { values = map (_ + inc) props.values }

ctlDecValues :: forall r. Number -> CtlProperties r -> CtlProperties r
ctlDecValues dec props = props { values = map (_ - dec) props.values }


-----------------------------------------------------------
-- LABEL CONTROL
-----------------------------------------------------------

type LabelCtlProperties =
  CtlProperties
  ( text         :: String
  , fontSizeInPx :: Number
  )


renderLabelCtl :: LabelCtlProperties -> VNode
renderLabelCtl props =
  let
    labelBox  = svgRect ["x":= props.x, "y":= props.y, "width":= props.width, "height":= props.height, "class":= "label-box"]
    labelText = svgText ["x" := props.x, "y" := props.y + 7, "class" := "label-text"] props.text
  in
    svgGroup ["id":= props.id, "transform":=("scale(" <> (show props.scale) <> ")")] [
      labelBox,
      labelText
      ]


-----------------------------------------------------------
-- BOOLEAN CONTROL
-----------------------------------------------------------

type BooleanCtlProperties =
  CtlProperties
  ( onValue  :: String
  , onColor  :: String
  , offValue :: String
  , offColor :: String
  )


renderBooleanCtl :: BooleanCtlProperties -> VNode
renderBooleanCtl props =
  let
    labelBox  = svgRect ["x":= props.x, "y":= props.y, "width":= 10, "height":= 10, "class":= "label-box", "style":= css["fill":booleanCtl_getColor props]]
  in
    svgGroup ["id":= props.id, "transform":=("scale(" <> (show props.scale) <> ")")] [
      labelBox
      ]


booleanCtl_getColor :: BooleanCtlProperties -> String
booleanCtl_getColor props =
  let
    v :: Number
    v = ctlValue props
  in
    if (show v) == props.onValue
      then props.onColor
      else props.offColor


-----------------------------------------------------------
-- GUAGE CONTROL
-----------------------------------------------------------

type GuageCtlProperties =
  CtlProperties
  ( desc     :: String
  , maxValue :: Int
  , value    :: Int
  )


renderGuageCtl :: GuageCtlProperties -> VNode
renderGuageCtl props =
  let
    outerBox     = svgRect ["id":= (props.id <> "2"), "x":= props.x, "y":= props.y, "width":= 20, "height":= props.maxValue + 2, "class":= "outerBox"]
    innerBox     = svgRect ["x":= props.x + 1, "y":= props.y + (props.maxValue - props.value) + 1, "width":= 18, "height":= props.value, "class":= "innerBox"]
    label        = svgText ["x":= props.x, "y":= props.y - 4, "class":= "indicator-label"] props.desc
    valueBox     = svgRect ["x":= props.x + 20 + 2, "y":= props.y + (props.maxValue / 2) - 5, "width":= 10, "height":= 10, "class":= "indicator-value-box"]
    valueBoxText = svgText ["x" := props.x + 20 + 3, "y" := props.y + (props.maxValue / 2) + 2, "class" := "indicator-value"] (show props.value)
  in
    svgGroup ["id":= props.id, "transform":=("scale(" <> (show props.scale) <> ")")] [
      label,
      outerBox,
      innerBox,
      valueBox,
      valueBoxText
      ]


data IndicatorCtl
  = LabelCtl LabelCtlProperties
  | BooleanCtl BooleanCtlProperties
  | GuageCtl GuageCtlProperties


indMap :: (forall r. CtlProperties r -> CtlProperties r) -> IndicatorCtl -> IndicatorCtl
indMap f (LabelCtl p)   = LabelCtl $ f p
indMap f (BooleanCtl p) = BooleanCtl $ f p
indMap f (GuageCtl p)   = GuageCtl $ f p


indGet :: forall a. (forall r. CtlProperties r -> a) -> IndicatorCtl -> a
indGet f (LabelCtl p)   = f p
indGet f (BooleanCtl p) = f p
indGet f (GuageCtl p)   = f p



indId :: IndicatorCtl -> String
indId (LabelCtl {id})   = id
indId (BooleanCtl {id}) = id
indId (GuageCtl {id})   = id

indSetScale :: Number -> IndicatorCtl -> IndicatorCtl
indSetScale scale = indMap (ctlSetScale scale)

indIncScale :: Number -> IndicatorCtl -> IndicatorCtl
indIncScale inc = indMap (ctlIncScale inc)

indDecScale :: Number -> IndicatorCtl -> IndicatorCtl
indDecScale dec = indMap (ctlDecScale dec)

indValue :: IndicatorCtl -> Number
indValue = indGet ctlValue

indValues :: IndicatorCtl -> Array Number
indValues = indGet _.values

indSetValues :: Array Number -> IndicatorCtl -> IndicatorCtl
indSetValues vs = indMap _ { values = vs }

indIncValues :: Number -> IndicatorCtl -> IndicatorCtl
indIncValues inc = indMap (ctlIncValues inc)

indDecValues :: Number -> IndicatorCtl -> IndicatorCtl
indDecValues dec = indMap (ctlDecValues dec)

indRender :: IndicatorCtl -> VNode
indRender (LabelCtl props)   = renderLabelCtl props
indRender (BooleanCtl props) = renderBooleanCtl props
indRender (GuageCtl props)   = renderGuageCtl props


getIndicators :: Unit -> Array IndicatorCtl
getIndicators _ =
  [ LabelCtl { id: "Bshop_oven_label", x: 0, y: 7, width: 100, height: 10, rotation: 0.0, scale: 1.0, orientation: Portrait, values: [], text: "B/shop oven", fontSizeInPx: 12.0 }
  , BooleanCtl { id: "Bshop_oven", x: 100, y: 70, width: 100, height: 10, rotation: 0.0, scale: 1.0, orientation: Portrait, values: [1.0], onValue:"1.0", onColor:"red", offValue:"0.0", offColor:"green" }
  , GuageCtl { id: "C02Buffer", x: 200, y: 70, width: 100, height: 10, rotation: 0.0, scale: 1.0, orientation: Portrait, values: [], desc: "C02 Buffer", maxValue: 20, value: 0 }
  ]

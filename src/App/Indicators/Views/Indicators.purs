module App.Indicators.Views.Indicators where

import Prelude
import Data.Int (round)
import Webfu.DOM
import Webfu.Mithril (VNode, Component, mkComponent, raise)
import Webfu.Mithril.HTML
import App.Indicators.Models


indRender :: Indicator -> VNode
indRender (LabelInd props)   = renderLabelInd props
indRender (BooleanInd props) = renderBooleanInd props
indRender (GuageInd props)   = renderGuageInd props
indRender (UnknownInd props) = renderUnknownInd props


-----------------------------------------------------------
-- LABEL CONTROL
-----------------------------------------------------------

renderLabelInd :: LabelIndProperties -> VNode
renderLabelInd props =
  let
    zIndexBox   = show props.z
    zIndexLabel = show $ props.z + 1
    labelBox  = svgRect ["x":= props.x, "y":= props.y, "width":= props.width, "height":= props.height, "class":= "label-box", "style":= css["z-index":zIndexBox]]
    labelText = svgText ["x" := props.x, "y" := props.y + 7, "class" := "label-text", "style":= css["z-index":zIndexLabel]] props.text
  in
    svgGroup ["id":= props.id, "transform":=("scale(" <> (show props.scale) <> ")"), "style":= css["z-index":zIndexBox]] [
      labelBox,
      labelText
      ]


-----------------------------------------------------------
-- BOOLEAN CONTROL
-----------------------------------------------------------

renderBooleanInd :: BooleanIndProperties -> VNode
renderBooleanInd props =
  let
    labelBox  = svgRect ["x":= props.x, "y":= props.y, "width":= 10, "height":= 10, "class":= "label-box", "style":= css["fill":blnGetColor' props]]
    labelText = svgText ["x" := props.x, "y" := props.y + 2, "class" := "label-text"] props.text
  in
    svgGroup ["id":= props.id, "transform":=("scale(" <> (show props.scale) <> ")"), "style":= css["z-index":(show props.z)]] [
      labelBox,
      labelText
      ]


-----------------------------------------------------------
-- GUAGE CONTROL
-----------------------------------------------------------

renderGuageInd :: GuageIndProperties -> VNode
renderGuageInd props =
  let
    zIndex       = show props.z
    zIndex1      = show $ props.z + 1
    zIndex2      = show $ props.z + 2
    outerBox     = svgRect ["x":= props.x, "y":= props.y, "width":= 20, "height":= (ggeIntMaxValue' props) + 2, "class":= "outerBox", "style":= css["z-index":zIndex]]
    innerBox     = svgRect ["x":= props.x + 1, "y":= props.y + ((ggeIntMaxValue' props) - (ggeIntValue' props)) + 1, "width":= 18, "height":= ggeIntValue' props, "class":= "innerBox", "style":= css["z-index":zIndex]]
    label        = svgText ["x":= props.x, "y":= props.y - 4, "class":= "indicator-label", "style":= css["z-index":zIndex]] props.text
    valueBox     = svgRect ["x":= props.x + 20 + 2, "y":= props.y + ((ggeIntMaxValue' props) / 2) - 5, "width":= 12, "height":= 10, "class":= "indicator-value-box", "style":= css["z-index":zIndex]]
    valueBoxText = svgText ["x":= props.x + 20 + 3, "y":= props.y + ((ggeIntMaxValue' props) / 2) + 2, "class":= "indicator-value", "style":= css["z-index":zIndex1]] (show $ ggeIntValue' props)
  in
    svgGroup ["id":= props.id, "transform":=("scale(" <> (show props.scale) <> ")"), "style":=("z-index:" <> zIndex)] [
      label,
      outerBox,
      innerBox,
      valueBox,
      valueBoxText
      ]


-----------------------------------------------------------
-- UNKNOWN CONTROL
-----------------------------------------------------------

renderUnknownInd :: forall r. IndProperties r -> VNode
renderUnknownInd props =
  let
    box  = svgRect ["x":= props.x, "y":= props.y, "width":= 10, "height":= 10, "class":= "label-box", "style":= css["fill":"black"]]
  in
    svgGroup ["id":= props.id, "transform":=("scale(" <> (show props.scale) <> ")")] [
      box
      ]

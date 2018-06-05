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


-----------------------------------------------------------
-- LABEL CONTROL
-----------------------------------------------------------

renderLabelInd :: LabelIndProperties -> VNode
renderLabelInd props =
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

renderBooleanInd :: BooleanIndProperties -> VNode
renderBooleanInd props =
  let
    labelBox  = svgRect ["x":= props.x, "y":= props.y, "width":= 10, "height":= 10, "class":= "label-box", "style":= css["fill":blnGetColor props]]
  in
    svgGroup ["id":= props.id, "transform":=("scale(" <> (show props.scale) <> ")")] [
      labelBox
      ]


-----------------------------------------------------------
-- GUAGE CONTROL
-----------------------------------------------------------

renderGuageInd :: GuageIndProperties -> VNode
renderGuageInd props =
  let
    outerBox     = svgRect ["id":= (props.id <> "2"), "x":= props.x, "y":= props.y, "width":= 20, "height":= (ggeIntMaxValue props) + 2, "class":= "outerBox"]
    innerBox     = svgRect ["x":= props.x + 1, "y":= props.y + ((ggeIntMaxValue props) - (indIntValue props)) + 1, "width":= 18, "height":= indIntValue props, "class":= "innerBox"]
    label        = svgText ["x":= props.x, "y":= props.y - 4, "class":= "indicator-label"] props.desc
    valueBox     = svgRect ["x":= props.x + 20 + 2, "y":= props.y + ((ggeIntMaxValue props) / 2) - 5, "width":= 12, "height":= 10, "class":= "indicator-value-box"]
    valueBoxText = svgText ["x" := props.x + 20 + 3, "y" := props.y + ((ggeIntMaxValue props) / 2) + 2, "class" := "indicator-value"] (show $ indValue props)
  in
    svgGroup ["id":= props.id, "transform":=("scale(" <> (show props.scale) <> ")")] [
      label,
      outerBox,
      innerBox,
      valueBox,
      valueBoxText
      ]

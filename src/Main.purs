module Main where

import Prelude
import Effect (Effect)
import Data.Maybe (fromJust)
import Foreign.Object (Object, empty, insert)
import Partial.Unsafe (unsafePartial)
import Webfu.DOM (doc_querySelector)
import Webfu.Mithril (Component, route) as M

import App.Indicators.Views.DisplayScreenView as DisplayScreenViewDisplayScreenView
import App.Indicators.Views.DisplayScreenPresenterM

-- import DOM.HTML (window) as DOM
-- import DOM.HTML.Window (document) as DOM
-- import DOM.Node.ParentNode (querySelector, QuerySelector(..)) as DOM
-- import DOM.HTML.Types (htmlDocumentToParentNode) as DOM


-- newtype Person = Person { name :: String, age :: Int }
-- derive instance genericPerson :: Generic Person

main :: Effect Unit
main = do
  body <- unsafePartial $ fromJust <$> doc_querySelector( "body" )
  rs <- routes
  M.route body "/Welcome" rs
  where
    routes :: Effect (Object M.Component)
    routes = do
      p <- mkDisplayScreenPresenter {indicators:[]}
      pure $ empty # insert "/Welcome" (DisplayScreenViewDisplayScreenView.mkView p)
  -- M.render body [M.mkTextVNode (Left "div#hw") {style:"color:red;"} "hello world"]


  -- let p = nodeChildNodes $ convert elem
  -- log (show $ length p)
  -- let p = unsafePartial fromJust $ (pure $ convert elem) >>= nodeParentNode >>= nodeParentNode
  -- log $ show $ nodeName p
  -- log $ show $ nodeType doc

  -- document <- DOM.window >>= DOM.document
  -- body <- unsafePartial (fromJust <$> DOM.querySelector (DOM.QuerySelector "body") (DOM.htmlDocumentToParentNode document))
  -- body <- unsafePartial $ fromJust (DOM.querySelector (DOM.QuerySelector "body") (DOM.htmlDocumentToParentNode document))
  -- let _ = route body "" M.empty
  -- pure unit

  -- log $ buildQueryString $ Person {name:"Rouan", age:10}
  -- log $ show $ toSpine $ Person {name:"Rouan", age:10}
  -- log $ show $ trust "na"
  -- log version

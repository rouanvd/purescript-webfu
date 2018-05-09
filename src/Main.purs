module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM, doc_querySelector)
import Data.Maybe (fromJust)
import Data.StrMap (StrMap, empty, insert)
import Mithril (Component, route) as M
import Partial.Unsafe (unsafePartial)

import Views.Welcome (mkView, state) as WelcomeView
import Views.Bukela (mkView, state) as BukelaView

-- import DOM.HTML (window) as DOM
-- import DOM.HTML.Window (document) as DOM
-- import DOM.Node.ParentNode (querySelector, QuerySelector(..)) as DOM
-- import DOM.HTML.Types (htmlDocumentToParentNode) as DOM


-- newtype Person = Person { name :: String, age :: Int }
-- derive instance genericPerson :: Generic Person

main :: forall e. Eff (console :: CONSOLE, dom :: DOM | e) Unit
main = do
  body <- unsafePartial $ fromJust <$> doc_querySelector( "body" )
  M.route body "/Welcome" routes
  where
    routes :: StrMap M.Component
    routes = empty # insert "/Welcome" (WelcomeView.mkView WelcomeView.state)
                   # insert "/Bukela" (BukelaView.mkView BukelaView.state)
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

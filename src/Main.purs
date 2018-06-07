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


main :: Effect Unit
main = do
  body <- unsafePartial $ fromJust <$> doc_querySelector( "body" )
  M.route body "/Welcome" routes
  where
    routes :: Object M.Component
    routes = empty # insert "/Welcome" (DisplayScreenViewDisplayScreenView.mkView (mkDisplayScreenPresenter {indicators:[]}))

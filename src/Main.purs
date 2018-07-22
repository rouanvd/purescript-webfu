module Main where

import Prelude
import Effect (Effect)
import Effect.Console
import Data.Maybe (fromJust)
import Foreign.Object (Object, empty, insert)
import Partial.Unsafe (unsafePartial)
import Webfu.DOM (doc_querySelector)
import Webfu.Mithril (Component, route) as M
import Webfu.DOM.Test
import Webfu.DOM.Promise

import App.Indicators.Views.DisplayScreenView as DisplayScreenView
import App.Indicators.Views.DisplayScreenPresenterM

import App.DragDrop.Views.DragDropView as DragDropView
import App.Tasks.Views.ListView as ListView
import App.Tasks.Views.ListPresenter as ListPresenter


main :: Effect Unit
main = do
  body <- unsafePartial $ fromJust <$> doc_querySelector( "body" )
  M.route body "/Welcome" routes
  pure unit
  where
    routes :: Object M.Component
    routes = empty # insert "/Welcome" (DisplayScreenView.mkView (mkDisplayScreenPresenter {indicators:[]}))
                   # insert "/DragDrop" (DragDropView.mkView unit)
                   # insert "/Todo" (ListView.mkView (ListPresenter.mkPresenter unit))

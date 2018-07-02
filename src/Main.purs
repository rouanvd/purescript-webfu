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

import App.Indicators.Views.DisplayScreenView as DisplayScreenViewDisplayScreenView
import App.Indicators.Views.DisplayScreenPresenterM

import App.DragDrop.Views.DragDropView as DragDropView


main :: Effect Unit
main = do
  body <- unsafePartial $ fromJust <$> doc_querySelector( "body" )
  M.route body "/Welcome" routes

  -- _ <- (pure $ requestPermission unit)
  --      >>= thn (\result -> log result)

  pure unit
  where
    routes :: Object M.Component
    routes = empty # insert "/Welcome" (DisplayScreenViewDisplayScreenView.mkView (mkDisplayScreenPresenter {indicators:[]}))
                   # insert "/DragDrop" (DragDropView.mkView unit)

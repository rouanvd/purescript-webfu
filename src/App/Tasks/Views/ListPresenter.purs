module App.Tasks.Views.ListPresenter
( Presenter
, mkPresenter
) where

import Prelude
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Webfu.Data.Err (Err(..))
import App.Tasks.Models (Task)


type Presenter =
  {}


mkPresenter :: Unit -> Presenter
mkPresenter initState =
  {}

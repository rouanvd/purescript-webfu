module App.Tasks.Views.ListPresenter
( Presenter
, mkPresenter
, addTask
) where

import Prelude
import Data.Array (snoc)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Webfu.Data.Err (Err(..))
import App.Tasks.Models (Task)
import App.GlobalState as GlobalState


type Presenter =
  { newTaskText :: String
  }


mkPresenter :: Unit -> Presenter
mkPresenter initState =
  { newTaskText: ""
  }


addTask :: Ref Presenter -> Effect Unit
addTask refPr = do
  pr <- Ref.read refPr
  let task = { title: pr.newTaskText, description: "" }
  Ref.modify_ (\tasks -> snoc tasks task ) GlobalState.tasks

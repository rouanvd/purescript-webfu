module App.Tasks.Views.ListView
( mkView
) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Array as Array
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref (read, write) as Ref
import Webfu.Data.Err (Err(..))
import Webfu.DOM
import Webfu.DOM.Promise
import Webfu.DOM.Events (KeyboardEvent)
import Webfu.DOM.Events.Keyboard (key)
import Webfu.Mithril (Component, VNode, mkComponent, raise, redraw)
import Webfu.Mithril.HTML
import App.GlobalState as GlobalState
import App.Tasks.Models (Task)
import App.Tasks.Views.ListPresenter (Presenter, addTask)


-----------------------------------------------------------
-- STATE
-----------------------------------------------------------


-----------------------------------------------------------
-- UPDATE
-----------------------------------------------------------
data Msg
  = AddTask
  | OnEnterKey KeyboardEvent

update :: Ref Presenter -> Msg -> Effect Unit
update refPr AddTask = pure unit
  -- maybeTaskTitle <- lookupValue "taskInput"
  -- case maybeTaskTitle of
  --   Nothing -> pure unit
  --   Just t  -> addTask t

update refPr (OnEnterKey e) =
  if (key e) == "Enter"
    then log "Enter baby"
    else pure unit


lookupValue :: String -> Effect (Maybe String)
lookupValue elemId = do
  elem <- doc_getElementById elemId
  pure $ el_prop_value =<< elem



-----------------------------------------------------------
-- VIEW
-----------------------------------------------------------

raiseEvent :: Ref Presenter -> Msg -> Unit
raiseEvent refSt = raise update refSt


mkView :: Presenter -> Component
mkView presenter =
  mkComponent presenter (\refSt _ -> do
    p <- Ref.read refSt
    tasks <- Ref.read GlobalState.tasks
    pure $ main [] [
      h1' ["style":="color:red;"] "TODOs",
      input ["type":="text", "id":="taskInput", onKeyPress \e -> pure $ raiseEvent refSt (OnEnterKey e)],
      button' ["onclick":= \_ -> raiseEvent refSt AddTask] "Add",
      table [] [
        tbody [] (map taskToRow tasks)
        ]
      ])

taskToRow :: Task -> VNode
taskToRow t =
  tr [] [
    td' [] t.title
    ]

module App.Tasks.Views.ListView
( mkView
) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref (read, write) as Ref
import Data.Maybe (Maybe(..), maybe)
import Webfu.Data.Err (Err(..))
import Webfu.DOM
import Webfu.DOM.Promise
import Webfu.Mithril (Component, mkComponent, raise, redraw)
import Webfu.Mithril.HTML
import App.Tasks.Views.ListPresenter (Presenter)

-----------------------------------------------------------
-- STATE
-----------------------------------------------------------


-----------------------------------------------------------
-- UPDATE
-----------------------------------------------------------
data Msg
  = AddTask

update :: Ref Presenter -> Msg -> Effect Unit
update refPr AddTask = pure unit


-----------------------------------------------------------
-- VIEW
-----------------------------------------------------------

raiseEvent :: Ref Presenter -> Msg -> Unit
raiseEvent refSt = raise update refSt


mkView :: Presenter -> Component
mkView presenter =
  mkComponent presenter (\refPr _ -> do
    p <- Ref.read refPr
    pure $ main [] [
      h1' ["style":="color:red;"] "TODOs",
      input [],
      button' ["onclick":= \_ -> raiseEvent refPr AddTask] "++"
      ])

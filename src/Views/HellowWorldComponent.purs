module Views.HelloWorldComponent (
  State,
  mkView
) where

import DOM
import Mithril.HTML
import Prelude hiding (div)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (Ref, newRef)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Array (find, findIndex, updateAt)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Mithril (VNode, Component, mkComponent, onInit, onRemove, raise)


-----------------------------------------------------------
-- STATE
-----------------------------------------------------------

type State = { name :: String }

-----------------------------------------------------------
-- UPDATE
-----------------------------------------------------------
data Msg

update :: forall eff. Msg -> State -> Eff (dom :: DOM |eff) State
update _ st = pure st



-----------------------------------------------------------
-- VIEW
-----------------------------------------------------------

-- raiseEvent :: Msg -> Unit
-- raiseEvent = raise update state


-- mkView :: forall eff. Unit -> Eff eff Component
mkView :: State -> Component
mkView state =
  mkComponent state (\stRef st _ ->
    h1' ["style":="color:red;"] ("Hello " <> st.name)
    )
    -- # onInit (\stRef st _ -> window >>= win_alert st.name)
    -- # onRemove (\stRef st _ -> window >>= win_alert "bye")

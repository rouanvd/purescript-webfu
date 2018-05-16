module Views.Welcome (
  state,
  mkView
) where

import Prelude (discard, Unit, show, pure, bind, ($), (+), (<>), (>>=))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (Ref, newRef)
import Webfu.Mithril (Component, mkComponent, raise)
import Webfu.Mithril.HTML
import Webfu.DOM (DOM, window, win_alert, typeError_message, typeError_name)
import Webfu.DOM.Fetch (responseStatusText, win_fetch)
import Webfu.DOM.Promise (Promise, mkPromise, then_, catch_, mkResolve, mkReject)


-----------------------------------------------------------
-- STATE
-----------------------------------------------------------

type State = {count :: Int}

state :: State
state = {count: 10}


-----------------------------------------------------------
-- UPDATE
-----------------------------------------------------------
data Msg = ButtonClick


update :: forall eff. Msg -> State -> Eff (console :: CONSOLE, dom :: DOM |eff) State
update ButtonClick st = do
  log "click baby!22"
  w <- window

  _ <- (win_fetch "http://localhost:8080/test.html" w)
       >>= (then_ (\r -> log ("Ok: " <> (responseStatusText r) )))
       >>= (catch_ (\te -> log ("Err: " <> (typeError_message te) <> " :: " <> (typeError_name te))))
  --     >>= (catch_ (\te -> log $ show te.))
  -- _ <- (mkPromise (\ resolveF rejectF -> rejectF "4"))
  --      >>= (then_ (\s -> log $ "ok: " <> s))
  --      >>= (catch_ (\s -> log $ "err: " <> s))
  
  pure $ {count: st.count + 2}



-----------------------------------------------------------
-- VIEW
-----------------------------------------------------------

raiseEvent :: Ref State -> Msg -> Unit
raiseEvent refSt = raise update refSt


-- mkView :: forall eff. Unit -> Eff eff Component
mkView :: State -> Component
mkView state =
  mkComponent state (\refSt st _ ->
    main [] [
      h1' ["style":="color:red;"] "my first application!",
      button' ["onclick":= \_ -> raiseEvent refSt ButtonClick] (show st.count),
      br []
      ])

module Webfu.DOM.Fetch
 ( win_fetch
 ) where

import Control.Monad.Eff (kind Effect, Eff)
import Webfu.DOM.Core
import Webfu.DOM.Promise


foreign import win_fetch_foreign :: forall eff. String -> Window -> Eff (dom :: DOM | eff) (Promise String String)

win_fetch :: forall eff. String -> Window -> Eff (dom :: DOM | eff) (Promise String String)
win_fetch url w = win_fetch_foreign url w



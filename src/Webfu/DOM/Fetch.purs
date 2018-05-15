module Webfu.DOM.Fetch
 ( Response
 , win_fetch
 ) where

import Control.Monad.Eff (kind Effect, Eff)
import Webfu.DOM.Core
import Webfu.DOM.Promise


data Response






foreign import win_fetch_foreign :: forall eff. String -> Window -> Eff (dom :: DOM | eff) (Promise Response TypeError)

win_fetch :: forall eff. String -> Window -> Eff (dom :: DOM | eff) (Promise Response TypeError)
win_fetch url w = win_fetch_foreign url w



module Webfu.DOM.Notification --Export List
  ( Notification
  , Undefined
  , NotificationOpts
  , module Webfu.DOM.Lang
  , notificationOpts
  , permission
  , lang
  , mkNotification
  , mkNotification'
  , body
  , dir
  , dataI
  , tag
  , icon
  , title
  , requestPermission
  , close
  ) where

import Prelude
import Webfu.DOM.Lang

import Data.Function.Uncurried (Fn0, Fn1, Fn10, Fn2, Fn3, runFn0, runFn1, runFn2, runFn3)
import Data.Maybe (Maybe)
import Effect (Effect)
import Foreign (Foreign)
import Webfu.DOM.Promise (Promise)
import Webfu.Data.Err (Err(..))
import Webfu.Data.ObjMap (Obj, Options, Option, options, (:=), empty)


foreign import data Notification :: Type
foreign import data Undefined :: Type

foreign import mkNotificationImpl :: Fn2 String Obj Notification

--Works
mkNotification :: String -> Notification
mkNotification s = runFn2 mkNotificationImpl s empty

--Works
mkNotification' :: String -> Options -> Notification
mkNotification' s opts = runFn2 mkNotificationImpl s (options opts)

--Works
foreign import permissionImpl :: Fn0 (Effect String)
permission :: Unit -> (Effect String)
permission _ = runFn0 permissionImpl

--Works
foreign import langImpl :: Fn1 Notification String
lang :: Notification -> String 
lang n = runFn1 langImpl n 

--Works
foreign import bodyImpl :: Fn1 Notification String
body :: Notification -> String
body n = runFn1 bodyImpl n

--Works
foreign import dirImpl :: Fn1 Notification String 
dir :: Notification -> String 
dir n = runFn1 dirImpl n

foreign import dataImpl :: Fn1 Notification Undefined
dataI :: Notification -> Undefined 
dataI n = runFn1 dataImpl n

--Works
foreign import tagImpl :: Fn1 Notification String 
tag :: Notification -> String 
tag n = runFn1 tagImpl n

--Works
foreign import iconImpl :: Fn1 Notification String 
icon :: Notification -> String 
icon n = runFn1 iconImpl n

--Works
foreign import titleImpl :: Fn1 Notification String 
title :: Notification -> String 
title n = runFn1 titleImpl n

--Works
foreign import requestPermissionImpl :: Fn0 (Promise String Err)
requestPermission :: Unit -> (Promise String Err)
requestPermission _ = runFn0 requestPermissionImpl

--Works
foreign import closeImpl :: Fn2 Notification Int (Effect Unit)
close :: Notification -> Int -> (Effect Unit)
close n t = runFn2 closeImpl n t

type NotificationOpts = 
  { dir :: Option String
  , lang :: Option Lang 
  , badge :: Option String
  , body :: Option String 
  , tag :: Option String
  , icon :: Option String
  , image :: Option String
  , optionalData :: Option Undefined
  }

notificationOpts :: NotificationOpts
notificationOpts = 
  { dir: ("dir" := _)
  , lang: \ v -> "lang" := show v
  , badge: ("badge" := _)
  , body: ("body" := _)
  , tag: ("tag" := _)
  , icon: ("icon" := _)
  , image: ("image" := _)
  , optionalData: ("optionalData" := _)
  }



 




 

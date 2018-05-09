module Data.ObjMap
  ( Obj
  , empty
  , allKeys
  , member
  , isSubObj
  , isEmpty
  , insert
  ) where

import Control.Monad.ST
import Data.Function.Uncurried
import Prelude

import Control.Monad.Eff (Eff, runPure, foreachE)


-- | `Obj` represents an object on which we can set properties & their values.
foreign import data Obj :: Type


foreign import _copyEff :: forall a b h r. a -> Eff (st :: ST h | r) b

-- | An empty map
foreign import empty :: Obj


-- | Test whether all keys in an `Obj` satisfy a predicate.
foreign import allKeys :: (String -> Boolean) -> Obj -> Boolean


foreign import _lookupKey :: forall a. Fn4 a (String -> a) String Obj a


-- | Test whether a `String` appears as a key in a map
member :: String -> Obj -> Boolean
member = runFn4 _lookupKey false (const true)


-- | Test whether one obj contains all of the keys contained in another obj
isSubObj :: Obj -> Obj -> Boolean
isSubObj o1 o2 = allKeys (\key1 -> member key1 o2) o1


-- | Test whether a map is empty
isEmpty :: Obj -> Boolean
isEmpty = allKeys (\_ -> false)


-- | Insert or replace a key/value pair in a map
foreign import _insert :: forall a. Fn3 Obj String a Obj
insert :: forall a. Obj -> String -> a -> Obj
insert obj key val = runFn3 _insert obj key val

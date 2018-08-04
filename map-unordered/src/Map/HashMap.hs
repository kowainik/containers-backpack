{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

module Map.HashMap
       ( Map
       , Key

       , empty
       , singleton

       , null
       , size
       , member
       , lookup
       , lookupDefault

       , toList
       , keys
       , elems

       , insert
       , insertWith
       , adjust
       , update
       , delete
       , alter
       ) where

import Prelude hiding (lookup, null)

import Data.Hashable (Hashable)

import qualified Data.HashMap.Strict as HM

----------------------------------------------------------------------------
-- Internals
----------------------------------------------------------------------------

class (Eq k, Hashable k) => Hash k
instance (Eq k, Hashable k) => Hash k

----------------------------------------------------------------------------
-- Implementation
----------------------------------------------------------------------------

type Map = HM.HashMap
type Key = Hash

empty :: Map k v
empty = HM.empty

singleton :: Key k => k -> v -> Map k v
singleton = HM.singleton

null :: Map k v -> Bool
null = HM.null

size :: Map k v -> Int
size = HM.size

member :: Key k => k -> Map k a -> Bool
member = HM.member

lookup :: Key k => k -> Map k v -> Maybe v
lookup = HM.lookup

lookupDefault :: Key k => v -> k -> Map k v -> v
lookupDefault = HM.lookupDefault

toList :: Map k v -> [(k, v)]
toList = HM.toList

keys :: Map k v -> [k]
keys = HM.keys

elems :: Map k v -> [v]
elems = HM.elems

insert :: Key k => k -> v -> Map k v -> Map k v
insert = HM.insert

insertWith :: Key k => (v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWith = HM.insertWith

adjust :: Key k => (a -> a) -> k -> Map k a -> Map k a
adjust = HM.adjust

update :: Key k => (a -> Maybe a) -> k -> Map k a -> Map k a
update = HM.update

delete :: Key k => k -> Map k v -> Map k v
delete = HM.delete

alter :: Key k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter = HM.alter

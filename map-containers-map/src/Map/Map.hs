{-# LANGUAGE ConstraintKinds #-}

module Map.Map
       ( Map
       , Key

       , empty
       , singleton
       , fromList

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

import qualified Data.Map as M

type Map = M.Map
type Key = Ord

empty :: Map k v
empty = M.empty

singleton :: Key k => k -> v -> Map k v
singleton = M.singleton

fromList :: Key k => [(k, v)] -> Map k v
fromList = M.fromList

null :: Map k v -> Bool
null = M.null

size :: Map k v -> Int
size = M.size

member :: Key k => k -> Map k a -> Bool
member = M.member

lookup :: Key k => k -> Map k v -> Maybe v
lookup = M.lookup

lookupDefault :: Key k => v -> k -> Map k v -> v
lookupDefault = M.findWithDefault

toList :: Map k v -> [(k, v)]
toList = M.toList

keys :: Map k v -> [k]
keys = M.keys

elems :: Map k v -> [v]
elems = M.elems

insert :: Key k => k -> v -> Map k v -> Map k v
insert = M.insert

insertWith :: Key k => (v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWith = M.insertWith

adjust :: Key k => (a -> a) -> k -> Map k a -> Map k a
adjust = M.adjust

update :: Key k => (a -> Maybe a) -> k -> Map k a -> Map k a
update = M.update

delete :: Key k => k -> Map k v -> Map k v
delete = M.delete

alter :: Key k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter = M.alter

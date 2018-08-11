{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations            #-}
{-# LANGUAGE TypeFamilies               #-}

module Map.Int
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

import qualified Data.IntMap.Strict as M

newtype Map k v = IM { unIM :: M.IntMap v }
    deriving newtype (Show, Eq)

class (k ~ Int) => Key k
instance (k ~ Int) => Key k

empty :: Key k => Map k v
empty = IM M.empty

singleton :: Key k => k -> v -> Map k v
singleton k = IM . M.singleton k

fromList :: Key k => [(k, v)] -> Map k v
fromList = IM . M.fromList

null :: Map k v -> Bool
null = M.null . unIM

size :: Map k v -> Int
size = M.size . unIM

member :: Key k => k -> Map k a -> Bool
member k = M.member k . unIM

lookup :: Key k => k -> Map k v -> Maybe v
lookup k = M.lookup k . unIM

lookupDefault :: Key k => v -> k -> Map k v -> v
lookupDefault def k = M.findWithDefault def k . unIM

toList :: Key k => Map k v -> [(k, v)]
toList = M.toList . unIM

keys :: Key k => Map k v -> [k]
keys = M.keys . unIM

elems :: Map k v -> [v]
elems = M.elems . unIM

insert :: Key k => k -> v -> Map k v -> Map k v
insert k v = IM . M.insert k v . unIM

insertWith :: Key k => (v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWith f k v= IM . M.insertWith f k v . unIM

adjust :: Key k => (a -> a) -> k -> Map k a -> Map k a
adjust f k = IM . M.adjust f k . unIM

update :: Key k => (a -> Maybe a) -> k -> Map k a -> Map k a
update f k = IM . M.update f k . unIM

delete :: Key k => k -> Map k v -> Map k v
delete k = IM . M.delete k . unIM

alter :: Key k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter f k = IM . M.alter f k . unIM

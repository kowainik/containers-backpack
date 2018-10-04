{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RoleAnnotations            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
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
import Data.Coerce (coerce)

import qualified Data.IntMap.Strict as M

newtype Map k v = IM (M.IntMap v)
    deriving newtype (Show, Eq)

type Key = (~) Int

empty :: forall k v. Map k v
empty = coerce @(M.IntMap v) M.empty

singleton :: Key k => k -> v -> Map k v
singleton k = coerce . M.singleton k

fromList :: Key k => [(k, v)] -> Map k v
fromList = coerce . M.fromList

null :: forall k v. Map k v -> Bool
null = M.null . coerce @_ @(M.IntMap v)

size :: forall k v. Map k v -> Int
size = M.size . coerce @_ @(M.IntMap v)

member :: forall k v. Key k => k -> Map k v -> Bool
member k = M.member k . coerce @_ @(M.IntMap v)

lookup :: Key k => k -> Map k v -> Maybe v
lookup k = M.lookup k . coerce

lookupDefault :: Key k => v -> k -> Map k v -> v
lookupDefault def k = M.findWithDefault def k . coerce

toList :: Key k => Map k v -> [(k, v)]
toList = M.toList . coerce

keys :: forall k v. Key k => Map k v -> [k]
keys = M.keys . coerce @_ @(M.IntMap v)

elems :: Map k v -> [v]
elems = M.elems . coerce

insert :: Key k => k -> v -> Map k v -> Map k v
insert k v = coerce . M.insert k v . coerce

insertWith :: Key k => (v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWith f k v= coerce . M.insertWith f k v . coerce

adjust :: Key k => (a -> a) -> k -> Map k a -> Map k a
adjust f k = coerce . M.adjust f k . coerce

update :: Key k => (a -> Maybe a) -> k -> Map k a -> Map k a
update f k = coerce . M.update f k . coerce

delete :: forall k v. Key k => k -> Map k v -> Map k v
delete k = coerce . M.delete k . coerce @_ @(M.IntMap v)

alter :: Key k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter f k = coerce . M.alter f k . coerce

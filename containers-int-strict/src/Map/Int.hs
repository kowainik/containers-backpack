{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

singleton :: forall k v. Key k => k -> v -> Map k v
singleton = coerce @(k -> v -> M.IntMap v) M.singleton

fromList :: forall k v. Key k => [(k, v)] -> Map k v
fromList = coerce @([(k, v)] -> M.IntMap v) M.fromList

null :: forall k v. Map k v -> Bool
null = coerce @(M.IntMap v -> Bool) M.null

size :: forall k v. Map k v -> Int
size = coerce @(M.IntMap v -> Int) M.size

member :: forall k v. Key k => k -> Map k v -> Bool
member = coerce @(k -> M.IntMap v -> Bool) M.member

lookup :: forall k v. Key k => k -> Map k v -> Maybe v
lookup = coerce @(k -> M.IntMap v -> Maybe v) M.lookup

lookupDefault :: forall k v. Key k => v -> k -> Map k v -> v
lookupDefault = coerce @(v -> k -> M.IntMap v -> v) M.findWithDefault

toList :: forall k v. Key k => Map k v -> [(k, v)]
toList = coerce @(M.IntMap v -> [(k, v)]) M.toList

keys :: forall k v. Key k => Map k v -> [k]
keys = coerce @(M.IntMap v -> [k]) M.keys

elems :: forall k v. Map k v -> [v]
elems = coerce @(M.IntMap v -> [v]) M.elems

insert :: forall k v. Key k => k -> v -> Map k v -> Map k v
insert = coerce @(k -> v -> M.IntMap v -> M.IntMap v) M.insert

insertWith :: forall k v. Key k => (v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWith = coerce @((v -> v -> v) -> k -> v -> M.IntMap v -> M.IntMap v) M.insertWith

adjust :: forall k v. Key k => (v -> v) -> k -> Map k v -> Map k v
adjust = coerce @((v -> v) -> k -> M.IntMap v -> M.IntMap v) M.adjust

update :: forall k v. Key k => (v -> Maybe v) -> k -> Map k v -> Map k v
update = coerce @((v -> Maybe v) -> k -> M.IntMap v -> M.IntMap v) M.update

delete :: forall k v. Key k => k -> Map k v -> Map k v
delete = coerce @(k -> M.IntMap v -> M.IntMap v) M.delete

alter :: forall k v. Key k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter = coerce @((Maybe v -> Maybe v) -> k -> M.IntMap v -> M.IntMap v) M.alter

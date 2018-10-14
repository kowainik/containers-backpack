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

type Key = (~) Int

empty :: Map k v
empty = IM M.empty
{-# INLINE empty #-}

singleton :: Key k => k -> v -> Map k v
singleton k = IM . M.singleton k
{-# INLINE singleton #-}

fromList :: Key k => [(k, v)] -> Map k v
fromList = IM . M.fromList
{-# INLINE fromList #-}

null :: Map k v -> Bool
null = M.null . unIM
{-# INLINE null #-}

size :: Map k v -> Int
size = M.size . unIM
{-# INLINE size #-}

member :: Key k => k -> Map k a -> Bool
member k = M.member k . unIM
{-# INLINE member #-}

lookup :: Key k => k -> Map k v -> Maybe v
lookup k = M.lookup k . unIM
{-# INLINE lookup #-}

lookupDefault :: Key k => v -> k -> Map k v -> v
lookupDefault def k = M.findWithDefault def k . unIM
{-# INLINE lookupDefault #-}

toList :: Key k => Map k v -> [(k, v)]
toList = M.toList . unIM
{-# INLINE toList #-}

keys :: Key k => Map k v -> [k]
keys = M.keys . unIM
{-# INLINE keys #-}

elems :: Map k v -> [v]
elems = M.elems . unIM
{-# INLINE elems #-}

insert :: Key k => k -> v -> Map k v -> Map k v
insert k v = IM . M.insert k v . unIM
{-# INLINE insert #-}

insertWith :: Key k => (v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWith f k v= IM . M.insertWith f k v . unIM
{-# INLINE insertWith #-}

adjust :: Key k => (a -> a) -> k -> Map k a -> Map k a
adjust f k = IM . M.adjust f k . unIM
{-# INLINE adjust #-}

update :: Key k => (a -> Maybe a) -> k -> Map k a -> Map k a
update f k = IM . M.update f k . unIM
{-# INLINE update #-}

delete :: Key k => k -> Map k v -> Map k v
delete k = IM . M.delete k . unIM
{-# INLINE delete #-}

alter :: Key k => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter f k = IM . M.alter f k . unIM
{-# INLINE alter #-}

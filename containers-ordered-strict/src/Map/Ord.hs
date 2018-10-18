{-# LANGUAGE ConstraintKinds #-}

module Map.Ord
       ( Map
       , Key

       , M.empty
       , singleton
       , M.fromList

       , M.null
       , M.size
       , M.member
       , M.lookup
       , lookupDefault

       , toList
       , keys
       , M.elems

       , M.insert
       , M.insertWith
       , M.adjust
       , M.update
       , M.delete
       , M.alter
       ) where

import Prelude hiding (lookup, null)

import qualified Data.Map.Strict as M

type Map = M.Map
type Key = Ord

lookupDefault :: Key k => v -> k -> Map k v -> v
lookupDefault = M.findWithDefault
{-# INLINE lookupDefault #-}

toList :: Key k => Map k v -> [(k, v)]
toList = M.toList
{-# INLINE toList #-}

keys :: Key k => Map k v -> [k]
keys = M.keys
{-# INLINE keys #-}

singleton :: Key k => k -> v -> Map k v
singleton = M.singleton
{-# INLINE singleton #-}

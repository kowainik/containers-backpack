{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Map.Prim
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
       ) where

import Prelude hiding (lookup, null)

import Data.Maybe (fromMaybe, isJust)

import qualified Data.Map.Lifted.Lifted as M
import qualified GHC.Exts as L

type Map = M.Map
type Key = Ord

empty :: Key k => Map k v
empty = fromList []

singleton :: Key k => k -> v -> Map k v
singleton = M.singleton

fromList :: Key k => [(k, v)] -> Map k v
fromList = L.fromList

null :: Map k v -> Bool
null = (== 0) . size

size :: Map k v -> Int
size = M.size

member :: Key k => k -> Map k a -> Bool
member k = isJust . lookup k

lookup :: Key k => k -> Map k v -> Maybe v
lookup = M.lookup

lookupDefault :: Key k => v -> k -> Map k v -> v
lookupDefault def k = fromMaybe def . lookup k

toList :: Key k => Map k v -> [(k, v)]
toList = L.toList

keys :: Key k => Map k v -> [k]
keys = map fst . toList

elems :: Map k v -> [v]
elems = M.foldrWithKey' cons []
  where
    cons :: k -> v -> [v] -> [v]
    cons _ = (:)

{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Map.Prim
       ( Map
       , Key

       , M.empty
       , singleton
       , M.fromList

       , null
       , M.size
       , member
       , M.lookup
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

singleton :: Key k => k -> v -> Map k v
singleton = M.singleton

null :: Map k v -> Bool
null = (== 0) . size

member :: Key k => k -> Map k a -> Bool
member k = isJust . M.lookup k

lookupDefault :: Key k => v -> k -> Map k v -> v
lookupDefault def k = fromMaybe def . M.lookup k

toList :: Key k => Map k v -> [(k, v)]
toList = toList

keys :: Key k => Map k v -> [k]
keys = map fst . toList

elems :: Map k v -> [v]
elems = M.foldrWithKey' cons []
  where
    cons :: k -> v -> [v] -> [v]
    cons _ = (:)

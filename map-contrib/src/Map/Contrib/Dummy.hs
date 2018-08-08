{-# LANGUAGE MonoLocalBinds #-}

module Map.Contrib.Dummy
       ( foo
       ) where

import Prelude hiding (lookup)

import Data.Maybe (isJust)

import Map (Key, Map, lookup, member)

foo :: Key k => k -> Map k v -> Bool
foo k m = member k m == isJust (lookup k m)

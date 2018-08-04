module Map.Contrib.Dummy where

import Prelude hiding (lookup)

import Data.Maybe (isJust)

import ROMap

foo :: Key k => k -> Map k v -> Bool
foo k m = member k m == isJust (lookup k m)

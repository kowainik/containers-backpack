{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Map.Contrib.Laws
       ( emptyInsertSingleton
       , insertDeleteEmpty
       , updateNothingRemoves
       , updateJustId
       , alterCanInsert
       ) where

import Map
import Prelude hiding (lookup, null)

emptyInsertSingleton :: (Key k, Eq k, Eq v) => k -> v -> Bool
emptyInsertSingleton k v = singleton k v == insert k v empty

insertDeleteEmpty :: (Key k, Eq k, Eq v) => k -> v -> Bool
insertDeleteEmpty k v = delete k (insert k v empty) == empty

updateNothingRemoves :: (Key k, Eq k, Eq v) => k -> v -> Bool
updateNothingRemoves k v = update (const Nothing) k (singleton k v) == empty

updateJustId :: (Key k, Eq k, Eq v) => k -> [(k, v)] -> Bool
updateJustId k pairs = update Just k m == m where m = fromList pairs

alterCanInsert :: (Key k, Eq k, Eq v) => k -> v -> Bool
alterCanInsert k v = alter (const (Just v)) k empty == singleton k v

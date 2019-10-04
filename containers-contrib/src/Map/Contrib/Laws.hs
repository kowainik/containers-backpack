{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Map.Contrib.Laws
       ( checkLaws
       ) where

import Data.Proxy (Proxy)
import Map (Key, alter, delete, empty, fromList, insert, singleton, update)
import Prelude hiding (lookup, null)
import Test.QuickCheck (Arbitrary, quickCheck)


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

checkLaws
    :: forall k v. (Key k, Arbitrary k, Arbitrary v, Show k, Show v, Eq k, Eq v)
    => Proxy k
    -> Proxy v
    -> IO ()
checkLaws _ _ = do
    quickCheck $ emptyInsertSingleton @k @v
    quickCheck $ insertDeleteEmpty @k @v
    quickCheck $ updateNothingRemoves @k @v
    quickCheck $ updateJustId @k @v
    quickCheck $ alterCanInsert @k @v

{-# LANGUAGE TypeFamilies        #-}

module Map.Contrib.Laws
       ( singletonFromList
       , nullEmpty
       , emptyZeroSized
       , singletonOneSized
       , memberEmptyFalse
       , newMemberExists
       , newMemberYieldsValidValue
       , lookupDefaultEmpty
       , listToSingleton
       , keysOfSingleton
       , elemsOfSingleton
       ) where

import Map
import Prelude hiding (lookup, null)

nullEmpty :: Bool
nullEmpty = null empty

emptyZeroSized :: Bool
emptyZeroSized = size empty == 0

singletonOneSized :: Key k => k -> v -> Bool
singletonOneSized k v = size (singleton k v) == 1

memberEmptyFalse :: Key k => k -> Bool
memberEmptyFalse k = not $ member k empty

newMemberExists :: Key k => k -> v -> Bool
newMemberExists k v = member k $ singleton k v

newMemberYieldsValidValue :: (Key k, Eq v) => k -> v -> Bool
newMemberYieldsValidValue k v = lookup k (singleton k v) == Just v

lookupDefaultEmpty :: (Key k, Eq v) => k -> v -> Bool
lookupDefaultEmpty k v = lookupDefault v k empty == v

listToSingleton :: (Key k, Eq k, Eq v) => k -> v -> Bool
listToSingleton k v = toList (singleton k v) == [(k, v)]

singletonFromList :: (Key k, Eq k, Eq v) => k -> v -> Bool
singletonFromList k v = singleton k v == fromList [(k, v)]

keysOfSingleton :: (Key k, Eq k, Eq v) => k -> v -> Bool
keysOfSingleton k v = keys (singleton k v) == [k]

elemsOfSingleton :: (Key k, Eq k, Eq v) => k -> v -> Bool
elemsOfSingleton k v = elems (singleton k v) == [v]

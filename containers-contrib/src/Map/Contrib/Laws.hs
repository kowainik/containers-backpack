{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Map.Contrib.Laws
       ( nullEmpty
       , nullImpliesZeroSize
       , nonZeroSizeImpliesNotNull
       , emptyZeroSized
       , singletonOneSized
       , memberEmptyFalse
       , newMemberExists
       , newMemberYieldsValidValue
       , lookupDefaultEmpty
       , listToSingleton
       , singletonFromList
       , keysOfSingleton
       , elemsOfSingleton
       , emptyInsertSingleton
       , insertDeleteEmpty
       , updateNothingRemoves
       , updateJustId
       , alterCanInsert
       ) where

import Map
import Prelude hiding (lookup, null)
import           Test.QuickCheck

nullEmpty :: Bool
nullEmpty = null empty

nullImpliesZeroSize :: Key k => Gen [(k, v)] -> Gen Property
nullImpliesZeroSize pairs = do
  m <- fromList <$> pairs
  pure $ null m ==> size m == 0

nonZeroSizeImpliesNotNull :: Key k => [(k, v)] -> Property
nonZeroSizeImpliesNotNull (fromList -> m) = size m > 0 ==> not (null m)

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

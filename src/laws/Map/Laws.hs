{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Map.Laws
       ( checkLaws

       -- * Exported for containers-example module. It demonstrates the law execution.
       , singletonFromList
       ) where

import Control.Monad (replicateM)
import Data.Proxy (Proxy)
import Map (Key, alter, delete, elems, empty, fromList, insert, keys, lookup, lookupDefault, member,
            null, singleton, size, toList, update)
import Prelude hiding (lookup, null)
import Test.QuickCheck (Arbitrary, Gen, Property, arbitrary, choose, quickCheck, (==>))


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

sizeIsNatural :: Key k => [(k, v)] -> Bool
sizeIsNatural (fromList -> m) = size m >= 0

singletonOneSized :: Key k => k -> v -> Bool
singletonOneSized k v = size (singleton k v) == 1

memberEmptyFalse :: Key k => k -> Bool
memberEmptyFalse k = not $ member k empty

memberSingletonSame :: Key k => k -> v -> Bool
memberSingletonSame k v = member k $ singleton k v

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

lookupMatchMember :: Key k => k -> [(k, v)] -> Bool
lookupMatchMember k (fromList -> m) = match (lookup k m) (member k m)
  where
    match :: Maybe v -> Bool -> Bool
    match Nothing False  = True
    match Nothing True   = False
    match (Just _) False = False
    match (Just _) True  = True

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

genSmallPairs :: (Key l, Arbitrary l, Arbitrary r) => Gen [(l, r)]
genSmallPairs = do
    len <- choose (0, 2)
    replicateM len $ (,) <$> arbitrary <*> arbitrary

checkLaws
    :: forall k v. (Key k, Arbitrary k, Arbitrary v, Show k, Show v, Eq k, Eq v)
    => Proxy k
    -> Proxy v
    -> IO ()
checkLaws _ _ = do
    -- query properties
    quickCheck nullEmpty
    quickCheck $ nullImpliesZeroSize (genSmallPairs @k @v)
    quickCheck $ nonZeroSizeImpliesNotNull @k @v
    quickCheck emptyZeroSized
    quickCheck $ sizeIsNatural @k @v
    quickCheck $ singletonOneSized @k @v
    quickCheck $ memberEmptyFalse @k
    quickCheck $ memberSingletonSame @k @v
    quickCheck $ newMemberYieldsValidValue @k @v
    quickCheck $ lookupDefaultEmpty @k @v
    quickCheck $ listToSingleton @k @v
    quickCheck $ singletonFromList @k @v
    quickCheck $ keysOfSingleton @k @v
    quickCheck $ elemsOfSingleton @k @v
    quickCheck $ lookupMatchMember @k @v

    -- update properties
    quickCheck $ emptyInsertSingleton @k @v
    quickCheck $ insertDeleteEmpty @k @v
    quickCheck $ updateNothingRemoves @k @v
    quickCheck $ updateJustId @k @v
    quickCheck $ alterCanInsert @k @v

{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad
import           Test.QuickCheck
import qualified Map.Contrib.Laws.Int as LI
import qualified Map.Contrib.Laws.RO.Int as LI
import qualified Map.Contrib.Laws.Ord as LO
import qualified Map.Contrib.Laws.RO.Ord as LO
import qualified Map.Contrib.Laws.Hash as LH
import qualified Map.Contrib.Laws.RO.Hash as LH
import qualified Map.Contrib.Laws.RO.Prim as PR

genSmallPairs :: (Arbitrary l, Arbitrary r) => Gen [(l, r)]
genSmallPairs = do
  len <- choose (0, 2)
  replicateM len $ (,) <$> arbitrary <*> arbitrary

main :: IO ()
main = do
  quickCheck PR.nullEmpty
  quickCheck $ PR.nullImpliesZeroSize (genSmallPairs @Int @String)
  quickCheck $ PR.nonZeroSizeImpliesNotNull @Int @String
  quickCheck PR.emptyZeroSized
  quickCheck $ PR.sizeIsNatural @Int @String
  quickCheck $ PR.singletonOneSized @Int @String
  quickCheck $ PR.memberEmptyFalse @Int
  quickCheck $ PR.memberSingletonSame @Int @String
  quickCheck $ PR.newMemberYieldsValidValue @Int @String
  quickCheck $ PR.lookupDefaultEmpty @Int @String
  quickCheck $ PR.listToSingleton @Int @String
  quickCheck $ PR.singletonFromList @Int @String
  quickCheck $ PR.keysOfSingleton @Int @String
  quickCheck $ PR.elemsOfSingleton @Int @String

  -- * Laws for Map.Int implementation
  quickCheck LI.nullEmpty
  quickCheck $ LI.nullImpliesZeroSize (genSmallPairs @Int @String)
  quickCheck $ LI.nonZeroSizeImpliesNotNull @Int @String
  quickCheck LI.emptyZeroSized
  quickCheck $ LI.sizeIsNatural @Int @String
  quickCheck $ LI.singletonOneSized @Int @String
  quickCheck $ LI.memberEmptyFalse @Int
  quickCheck $ LI.memberSingletonSame @Int @String
  quickCheck $ LI.newMemberYieldsValidValue @Int @String
  quickCheck $ LI.lookupDefaultEmpty @Int @String
  quickCheck $ LI.listToSingleton @Int @String
  quickCheck $ LI.singletonFromList @Int @String
  quickCheck $ LI.keysOfSingleton @Int @String
  quickCheck $ LI.elemsOfSingleton @Int @String
  quickCheck $ LI.emptyInsertSingleton @Int @String
  quickCheck $ LI.insertDeleteEmpty @Int @String
  quickCheck $ LI.updateNothingRemoves @Int @String
  quickCheck $ LI.updateJustId @Int @String
  quickCheck $ LI.alterCanInsert @Int @String

  -- * Laws for Map.Ord implementation
  quickCheck LO.nullEmpty
  quickCheck $ LO.nullImpliesZeroSize (genSmallPairs @Int @String)
  quickCheck $ LO.nonZeroSizeImpliesNotNull @Int @String
  quickCheck LO.emptyZeroSized
  quickCheck $ LO.sizeIsNatural @String @String
  quickCheck $ LO.singletonOneSized @String @String
  quickCheck $ LO.memberEmptyFalse @String
  quickCheck $ LO.memberSingletonSame @String @String
  quickCheck $ LO.newMemberYieldsValidValue @String @String
  quickCheck $ LO.lookupDefaultEmpty @String @String
  quickCheck $ LO.listToSingleton @String @String
  quickCheck $ LO.singletonFromList @String @String
  quickCheck $ LO.keysOfSingleton @String @String
  quickCheck $ LO.elemsOfSingleton @String @String
  quickCheck $ LO.emptyInsertSingleton @String @String
  quickCheck $ LO.insertDeleteEmpty @String @String
  quickCheck $ LO.updateNothingRemoves @String @String
  quickCheck $ LO.updateJustId @String @String
  quickCheck $ LO.alterCanInsert @String @String

  -- * Laws for Map.Hash implementation
  quickCheck LH.nullEmpty
  quickCheck $ LH.nullImpliesZeroSize (genSmallPairs @Int @String)
  quickCheck $ LH.nonZeroSizeImpliesNotNull @String @String
  quickCheck LH.emptyZeroSized
  quickCheck $ LH.sizeIsNatural @String @String
  quickCheck $ LH.singletonOneSized @String @String
  quickCheck $ LH.memberEmptyFalse @String
  quickCheck $ LH.memberSingletonSame @String @String
  quickCheck $ LH.newMemberYieldsValidValue @String @String
  quickCheck $ LH.lookupDefaultEmpty @String @String
  quickCheck $ LH.listToSingleton @String @String
  quickCheck $ LH.singletonFromList @String @String
  quickCheck $ LH.keysOfSingleton @String @String
  quickCheck $ LH.elemsOfSingleton @String @String
  quickCheck $ LH.emptyInsertSingleton @String @String
  quickCheck $ LH.insertDeleteEmpty @String @String
  quickCheck $ LH.updateNothingRemoves @String @String
  quickCheck $ LH.updateJustId @String @String
  quickCheck $ LH.alterCanInsert @String @String

{-# LANGUAGE TypeApplications #-}

module Main where

import           Test.QuickCheck
import qualified Map.Contrib.Laws.Int as LI
import qualified Map.Contrib.Laws.Ord as LO
import qualified Map.Contrib.Laws.Hash as LH

main :: IO ()
main = do
  -- * Laws for Map.Int implementation
  quickCheck LI.nullEmpty
  quickCheck LI.emptyZeroSized
  quickCheck (LI.singletonOneSized @Int @String)
  quickCheck (LI.memberEmptyFalse @Int)
  quickCheck (LI.newMemberExists @Int @String)
  quickCheck (LI.newMemberYieldsValidValue @Int @String)
  quickCheck (LI.lookupDefaultEmpty @Int @String)
  quickCheck (LI.listToSingleton @Int @String)
  quickCheck (LI.singletonFromList @Int @String)
  quickCheck (LI.keysOfSingleton @Int @String)
  quickCheck (LI.elemsOfSingleton @Int @String)
  quickCheck (LI.emptyInsertSingleton @Int @String)
  quickCheck (LI.insertDeleteEmpty @Int @String)
  quickCheck (LI.updateNothingRemoves @Int @String)
  quickCheck (LI.updateJustId @Int @String)
  quickCheck (LI.alterCanInsert @Int @String)

  -- * Laws for Map.Ord implementation
  quickCheck LO.nullEmpty
  quickCheck LO.emptyZeroSized
  quickCheck (LO.singletonOneSized @String @String)
  quickCheck (LO.memberEmptyFalse @String)
  quickCheck (LO.newMemberExists @String @String)
  quickCheck (LO.newMemberYieldsValidValue @String @String)
  quickCheck (LO.lookupDefaultEmpty @String @String)
  quickCheck (LO.listToSingleton @String @String)
  quickCheck (LO.singletonFromList @String @String)
  quickCheck (LO.keysOfSingleton @String @String)
  quickCheck (LO.elemsOfSingleton @String @String)
  quickCheck (LO.emptyInsertSingleton @String @String)
  quickCheck (LO.insertDeleteEmpty @String @String)
  quickCheck (LO.updateNothingRemoves @String @String)
  quickCheck (LO.updateJustId @String @String)
  quickCheck (LO.alterCanInsert @String @String)

  -- * Laws for Map.Hash implementation
  quickCheck LH.nullEmpty
  quickCheck LH.emptyZeroSized
  quickCheck (LH.singletonOneSized @String @String)
  quickCheck (LH.memberEmptyFalse @String)
  quickCheck (LH.newMemberExists @String @String)
  quickCheck (LH.newMemberYieldsValidValue @String @String)
  quickCheck (LH.lookupDefaultEmpty @String @String)
  quickCheck (LH.listToSingleton @String @String)
  quickCheck (LH.singletonFromList @String @String)
  quickCheck (LH.keysOfSingleton @String @String)
  quickCheck (LH.elemsOfSingleton @String @String)
  quickCheck (LH.emptyInsertSingleton @String @String)
  quickCheck (LH.insertDeleteEmpty @String @String)
  quickCheck (LH.updateNothingRemoves @String @String)
  quickCheck (LH.updateJustId @String @String)
  quickCheck (LH.alterCanInsert @String @String)

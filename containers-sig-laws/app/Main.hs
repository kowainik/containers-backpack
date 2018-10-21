{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Map.Contrib.Laws.RO.Prim as ROPR
import qualified Map.Contrib.Laws.Int as LI
import qualified Map.Contrib.Laws.RO.Int as ROLI
import qualified Map.Contrib.Laws.Ord as LO
import qualified Map.Contrib.Laws.RO.Ord as ROLO
import qualified Map.Contrib.Laws.Hash as LH
import qualified Map.Contrib.Laws.RO.Hash as ROLH
import Data.Proxy

main :: IO ()
main = do
  -- * Laws for Map.Prim implementation
  ROPR.checkLaws (Proxy @String) (Proxy @String)

  -- * Laws for Map.Int implementation
  LI.checkLaws (Proxy @Int) (Proxy @String)
  ROLI.checkLaws (Proxy @Int) (Proxy @String)

  -- * Laws for Map.Ord implementation
  LO.checkLaws (Proxy @Int) (Proxy @String)
  ROLO.checkLaws (Proxy @Int) (Proxy @String)

  -- * Laws for Map.Hash implementation
  LH.checkLaws (Proxy @Int) (Proxy @String)
  ROLH.checkLaws (Proxy @Int) (Proxy @String)

{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Map.Contrib.Bench.Int.RO as BIRO
import qualified Map.Contrib.Bench.Ord.RO as BORO
import qualified Map.Contrib.Bench.Hash.RO as BHRO
import Data.Proxy (Proxy (..))

-- Map key type for benchmarks. It can be Int, Char or any
-- type that obeys to the constraint:
-- (Enum k, Bounded k, Key k, NFData (Map k Int))
type K = Int

main :: IO ()
main = do
  BIRO.benchmark "intmap" (Proxy @K)

  BORO.benchmark "ordmap" (Proxy @K)

  BHRO.benchmark "hashmap" (Proxy @K)

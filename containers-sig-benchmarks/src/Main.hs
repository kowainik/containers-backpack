{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Map.Contrib.Bench.Int.RO as BIRO
import qualified Map.Contrib.Bench.Ord.RO as BORO
import qualified Map.Contrib.Bench.Hash.RO as BHRO

-- Map key type for benchmarks. It can be Int, Char or any
-- type that obeys to the constraint:
-- (Enum k, Bounded k, Key k, NFData (Map k Int))
type K = Int

main :: IO ()
main = do
  BIRO.benchmark @K "intmap"

  BORO.benchmark @K "ordmap"

  BHRO.benchmark @K "hashmap"

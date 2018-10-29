module Main where

import Gauge.Main (defaultMain)

import qualified Map.Contrib.Bench.Int.RO as BIRO
import qualified Map.Contrib.Bench.Ord.RO as BORO
import qualified Map.Contrib.Bench.Hash.RO as BHRO

-- Map key type for benchmarks. It can be Int, Char or any
-- type that obeys to the constraint:
-- (Enum k, Bounded k, Key k, NFData k)
type K = Int

main :: IO ()
main = do
  let
    lookupRangeLower = 0
    lookupRangeUpper = 1024

    -- Apply bounds to the given benchmark function
    apply_bounds f =
      f (lookupRangeLower, lookupRangeUpper)

  defaultMain =<<
    sequence
      [ apply_bounds BIRO.benchmark "intmap"
      , apply_bounds BORO.benchmark "ordmap"
      , apply_bounds BHRO.benchmark "hashmap"
      ]

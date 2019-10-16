module Main (main) where

import Gauge.Main (defaultMain)

import qualified Map.Bench.Hash as Hash
import qualified Map.Bench.Int as Int
import qualified Map.Bench.Ord as Ord


main :: IO ()
main = do
    let lookupRangeLower :: Int
        lookupRangeLower = 0
    let lookupRangeUpper :: Int
        lookupRangeUpper = 1024

    -- Apply bounds to the given benchmark function
    let applyBounds :: ((Int, Int) -> a) -> a
        applyBounds f = f (lookupRangeLower, lookupRangeUpper)

    defaultMain =<< sequence
        [ applyBounds Int.simpleBenchmark  "intmap"
        , applyBounds Ord.simpleBenchmark  "ordmap"
        , applyBounds Hash.simpleBenchmark "hashmap"
        ]

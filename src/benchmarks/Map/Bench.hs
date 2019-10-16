{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TypeFamilies        #-}

module Map.Bench
       ( simpleBenchmark
       ) where

import Relude hiding (Map, fromList)

import Gauge.Main (Benchmark, bench, bgroup, whnf)

import Map (Key, Map, fromList, lookup)


simpleBenchmark
    :: forall k .
       (Enum k, Key k, NFData (Map k Int))
    => (k, k)
    -> String
    -> IO Benchmark
simpleBenchmark (lower, upper) structureName = do
    let map_keys :: [k] = [lower..upper]

    let mapEntries = zip map_keys [0..]

    let m = fromList mapEntries

    evaluateNF_ m

    pure $ bgroup structureName
        [ lookupBench ("lookup/all") m (map fst mapEntries)
        ]

lookupBench :: forall k. Key k => String -> Map k Int -> [k] -> Benchmark
lookupBench structureName m = bench structureName . whnf (go 0)
  where
    go :: Int -> [k] -> Int
    go = foldl' (\a -> (a +) . fromMaybe 0 . flip lookup m)

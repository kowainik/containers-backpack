{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Map.Contrib.Bench where

import Map
import Gauge.Main (Benchmark, bench, defaultMain, whnf)
import Relude hiding (fromList, Map)

benchmark
  :: forall k. (Enum k, Bounded k, Key k, NFData (Map k Int))
  => String
  -> Proxy k
  -> IO ()
benchmark label _ = do
  let benchMaxElems = min 1024 (fromEnum (maxBound @k))
  let upBound = foldl' (.) id (replicate benchMaxElems succ) minBound
  let map_keys :: [k] = [minBound..upBound]

  let mapEntries = zip map_keys [0..]

  let m = fromList mapEntries

  evaluateNF_ m

  defaultMain
    [ lookupBench (label <> "-lookup/all") m (map fst mapEntries)
    ]

lookupBench :: forall k. Key k => String -> Map k Int -> [k] -> Benchmark
lookupBench label m = bench label . whnf (go 0)
  where
    go :: Int -> [k] -> Int
    go = foldl' (\a -> (a +) . fromMaybe 0 . flip lookup m)

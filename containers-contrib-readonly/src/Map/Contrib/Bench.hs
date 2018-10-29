{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Map.Contrib.Bench where

import Gauge.Main (Benchmark, bench, whnf, bgroup)
import Relude hiding (fromList, Map)

import Map

benchmark
  :: forall k. (Enum k, Bounded k, Key k, NFData k)
  => (k, k)
  -> String
  -> IO Benchmark
benchmark (lower, upper) label = do
  let map_keys :: [k] = [lower..upper]

  let mapEntries = zip map_keys [0..]

  let m = fromList mapEntries

  evaluateNF_ m

  pure $
    bgroup
      label
      [ lookupBench ("lookup/all") m (map fst mapEntries)
      ]

lookupBench :: forall k. Key k => String -> Map k Int -> [k] -> Benchmark
lookupBench label m = bench label . whnf (go 0)
  where
    go :: Int -> [k] -> Int
    go = foldl' (\a -> (a +) . fromMaybe 0 . flip lookup m)

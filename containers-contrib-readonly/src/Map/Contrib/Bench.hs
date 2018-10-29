{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Map.Contrib.Bench
  ( benchmark
  ) where

import Gauge.Main (Benchmark, bench, whnf, bgroup)
import Relude hiding (fromList, Map)

import Map

benchmark
  :: forall k. (Enum k, Bounded k, Key k, NFData k)
  => (k, k)
  -> String
  -> IO Benchmark
benchmark (lower, upper) structureName = do
  let map_keys :: [k] = [lower..upper]

  let mapEntries = zip map_keys [0..]

  let m = fromList mapEntries

  evaluateNF_ m

  pure $
    bgroup
      structureName
      [ lookupBench ("lookup/all") m (map fst mapEntries)
      ]

lookupBench :: forall k. Key k => String -> Map k Int -> [k] -> Benchmark
lookupBench structureName m = bench structureName . whnf (go 0)
  where
    go :: Int -> [k] -> Int
    go = foldl' (\a -> (a +) . fromMaybe 0 . flip lookup m)

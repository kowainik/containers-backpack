module Dummy where

import Map.Contrib.Dummy.IntMap (foo)

import Map.IntMap as M

x = foo (1 :: Int) (M.singleton 1 ())

{-# LANGUAGE ConstraintKinds #-}

module Map.Ord
       ( Map
       , Key

       , M.empty
       , M.singleton
       , M.fromList

       , M.null
       , M.size
       , M.member
       , M.lookup
       , M.findWithDefault

       , M.toList
       , M.keys
       , M.elems

       , M.insert
       , M.insertWith
       , M.adjust
       , M.update
       , M.delete
       , M.alter
       ) where

import Prelude hiding (lookup, null)

import qualified Data.Map.Strict as M

type Map = M.Map
type Key = Ord

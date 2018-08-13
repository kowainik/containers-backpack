{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Map.Contrib.Group
       ( groupBy
       , groupOneBy
       ) where

import Prelude hiding (lookup)

import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..), (<|))

import Map (Key, Map, alter, empty)

{- | Groups elements using results of the given function as keys.

>>> groupBy even [1..6]
fromList [(False,5 :| [3,1]),(True,6 :| [4,2])]
-}
groupBy :: forall f k a . (Foldable f, Key k)
        => (a -> k) -> f a -> Map k (NonEmpty a)
groupBy f = foldl' mapGroup empty
  where
    mapGroup :: Map k (NonEmpty a) -> a -> Map k (NonEmpty a)
    mapGroup m a =
        let val :: Maybe (NonEmpty a) -> NonEmpty a
            val Nothing   = a :| []
            val (Just xs) = a <| xs
        in alter (Just . val) (f a) m

{- | Similar to 'groupBy' but keeps only one element as value.

>>> groupOneBy even [1 .. 6]
fromList [(False,1),(True,2)]
-}
groupOneBy :: forall f k a . (Foldable f, Key k)
           => (a -> k) -> f a -> Map k a
groupOneBy f = foldl' mapGroup empty
  where
    mapGroup :: Map k a -> a -> Map k a
    mapGroup m a =
        let val :: Maybe a -> a
            val Nothing  = a
            val (Just x) = x
        in alter (Just . val) (f a) m

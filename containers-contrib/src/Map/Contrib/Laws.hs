{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeFamilies   #-}

module Map.Contrib.Laws
       ( singletonFromList
       ) where

import Map (Key, fromList, singleton)

singletonFromList :: (Key k, Eq k, Eq v) => k -> v -> Bool
singletonFromList k v = singleton k v == fromList [(k, v)]

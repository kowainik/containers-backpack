{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Hashable (Hashable)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Map.Contrib.Laws.Hash as LH
import qualified Map.Contrib.Laws.Int as LI
import qualified Map.Contrib.Laws.Ord as LO
import qualified Map.Contrib.Laws.RO.Hash as ROLH
import qualified Map.Contrib.Laws.RO.Int as ROLI
import qualified Map.Contrib.Laws.RO.Ord as ROLO
import qualified Map.Contrib.Laws.RO.Prim as ROPR
import Test.QuickCheck (Arbitrary, arbitrary, choose, vectorOf)

-- | ShortText is a Text with length [0, 3] chosen "arbitrarily"
data ShortText = ShortText T.Text
  deriving (Show, Eq, Ord, Generic, Hashable)

-- | Generates a Text with length [0, 3]. Choosing a small text speeds up the quickCheck tests.
-- Note: do not make minimum larger than 0, empty texts are often edge cases.
instance Arbitrary ShortText where
  arbitrary = ShortText . T.pack <$> (choose (0, 3) >>= flip vectorOf arbitrary)

type K = Int
type V = ShortText

main :: IO ()
main = do
  -- * Laws for Map.Prim implementation
  ROPR.checkLaws (Proxy @K) (Proxy @V)

  -- * Laws for Map.T.Text implementation
  LI.checkLaws (Proxy @Int) (Proxy @V)
  ROLI.checkLaws (Proxy @Int) (Proxy @V)

  -- * Laws for Map.Ord implementation
  LO.checkLaws (Proxy @K) (Proxy @V)
  ROLO.checkLaws (Proxy @K) (Proxy @V)

  -- * Laws for Map.Hash implementation
  LH.checkLaws (Proxy @K) (Proxy @V)
  ROLH.checkLaws (Proxy @K) (Proxy @V)

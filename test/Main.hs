{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import Data.Hashable (Hashable)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, choose, vectorOf)

import qualified Data.Text as T
import qualified Map.Laws.Hash as Hash
import qualified Map.Laws.Int as Int
import qualified Map.Laws.Ord as Ord


-- | ShortText is a Text with length [0, 3] chosen "arbitrarily"
newtype ShortText = ShortText
    { unShortText :: Text
    } deriving stock (Show, Generic)
      deriving newtype (Eq, Ord, Hashable)

{- | Generates a Text with length [0, 3]. Choosing a small text speeds up the
quickCheck tests.

Note: do not make minimum larger than 0, empty texts are often edge cases.
-}
instance Arbitrary ShortText where
    arbitrary :: Gen ShortText
    arbitrary = ShortText . T.pack <$> (choose (0, 3) >>= flip vectorOf arbitrary)

type K = Int
type V = ShortText

main :: IO ()
main = do
    Int.checkLaws (Proxy @Int) (Proxy @V)
    Ord.checkLaws (Proxy @K) (Proxy @V)
    Hash.checkLaws (Proxy @K) (Proxy @V)
    -- Prim.checkLaws (Proxy @K) (Proxy @V)

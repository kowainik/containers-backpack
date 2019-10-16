module Main (main) where

import qualified Map.Contrib.Group.Hash as HM (groupBy)
import qualified Map.Contrib.Group.Int as IM (groupBy)
import qualified Map.Contrib.Group.Ord as M (groupBy)


main :: IO ()
main = do
    putStrLn "### IntMap ###"
    print $ IM.groupBy (`mod` 2) [1..10]

    putStrLn "### Map ###"
    print $ M.groupBy (`mod` 2) ([1..10] :: [Int])

    putStrLn "### HashMap ###"
    print $ HM.groupBy (`mod` 2) ([1..10] :: [Int])

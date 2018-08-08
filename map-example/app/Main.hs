module Main where


import qualified Map.Contrib.Group.HashMap as HM (groupBy)
import qualified Map.Contrib.Group.IntMap as IM (groupBy)
import qualified Map.Contrib.Group.Map as M (groupBy)


main :: IO ()
main = do
    putStrLn "### IntMap ###"
    print $ IM.groupBy (`mod` 2) [1..10]
    putStrLn "### Map ###"
    print $ M.groupBy (`mod` 2) ([1..10] :: [Int])
    putStrLn "### HashMap ###"
    print $ HM.groupBy (`mod` 2) ([1..10] :: [Int])

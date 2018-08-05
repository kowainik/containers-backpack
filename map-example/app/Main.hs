module Main where


import Map.Contrib.Group.IntMap (groupBy)


main :: IO ()
main = print $ groupBy (`mod` 2) [1..10]

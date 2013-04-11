module Data.CRF.Chain2.Tiers.Util
( partition
) where

import Data.List (transpose)

partition :: Int -> [a] -> [[a]]
partition n =
    transpose . group n
  where
    group _ [] = []
    group k xs = take k xs : (group k $ drop k xs)

module JosephRing where

joseph 1 _ = 1
joseph n m = (joseph (n - 1) m + m - 1) `mod` n + 1

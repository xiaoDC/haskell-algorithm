module MergeSort where

msort :: (Ord a) => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge ll rl
 where
  ll = msort (take (div (length xs) 2) xs)
  rl = msort $ drop (div (length xs) 2) xs


merge :: (Ord a) => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge xl@(x : xs) yl@(y : ys) | x <= y    = x : merge xs yl
                              | otherwise = y : merge xl ys

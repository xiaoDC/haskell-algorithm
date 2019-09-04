module SelectSort where
import Data.List (minimum, delete)

selectSort :: (Ord a) => [a] -> [a]
selectSort [] = []
selectSort xs = let x = minimum xs in x : selectSort (delete x xs)

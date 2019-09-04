module InsertSort where

isort :: (Ord a) => [a] -> [a]
isort [] = []
isort xs = foldl insert2arr [] xs

insert2arr :: (Ord a) => [a] -> a -> [a]
insert2arr [] x = [x]
insert2arr (x : xs) y | y <= x    = y : x : xs
                      | otherwise = x : insert2arr xs y

module BTree where

data BTree a = Nil | BTree a (BTree a) (BTree a) deriving (Show)

insert :: (Ord a) => BTree a -> a -> BTree a
insert Nil x = BTree x Nil Nil
insert (BTree a lt rt) x
  | x <= a = BTree a (insert lt x) rt
  | otherwise = BTree a lt (insert rt x)


deleteMaxNode :: (Ord a) => BTree a -> (BTree a, a)
deleteMaxNode (BTree a Nil Nil) = (Nil, a)
deleteMaxNode (BTree a lt Nil) = (lt, a)
deleteMaxNode (BTree a lt rt) = (BTree a lt drt, x) where (drt, x) = deleteMaxNode rt

mergeTree :: (Ord a) => BTree a -> BTree a -> BTree a
mergeTree Nil x = x
mergeTree x Nil = x
mergeTree lt rt = (BTree x nlt rt) where (nlt, x) = deleteMaxNode lt


delete :: (Ord a) => BTree a -> a -> BTree a
delete Nil _ = Nil
delete (BTree v lt rt) x
  | x == v = mergeTree lt rt
  | x < v = (BTree v nlt rt)
  | otherwise = (BTree v lt nrt)
  where nlt = delete lt x
        nrt = delete rt x

search :: (Ord a) => BTree a -> a -> Bool
search Nil _ = False
search (BTree a lt rt) x
  | x == a = True
  | x < a = search lt x
  | x > a = search rt x

initTree :: (Ord a) => [] a -> BTree a
initTree [] = Nil
initTree xs = foldl insert Nil xs

-- let a = Nil
-- b = insert a 6
-- c = insert b 3
-- d = insert c 4
-- e = insert d 9
-- let x = initTree [6, 2, 3, 1, 7, 9, 1]

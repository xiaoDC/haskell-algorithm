module BSTree where

data BSTree a = Nil | BSTree a (BSTree a) (BSTree a) deriving (Show)

insert :: (Ord a) => BSTree a -> a -> BSTree a
insert Nil x = BSTree x Nil Nil
insert (BSTree a lt rt) x | x <= a    = BSTree a (insert lt x) rt
                          | otherwise = BSTree a lt (insert rt x)


deleteMaxNode :: (Ord a) => BSTree a -> (BSTree a, a)
deleteMaxNode (BSTree a Nil Nil) = (Nil, a)
deleteMaxNode (BSTree a lt  Nil) = (lt, a)
deleteMaxNode (BSTree a lt  rt ) = (BSTree a lt drt, x)
  where (drt, x) = deleteMaxNode rt

mergeTree :: (Ord a) => BSTree a -> BSTree a -> BSTree a
mergeTree Nil x   = x
mergeTree x   Nil = x
mergeTree lt  rt  = BSTree x nlt rt where (nlt, x) = deleteMaxNode lt


delete :: (Ord a) => BSTree a -> a -> BSTree a
delete Nil _ = Nil
delete (BSTree v lt rt) x | x == v    = mergeTree lt rt
                          | x < v     = BSTree v nlt rt
                          | otherwise = BSTree v lt nrt
 where
  nlt = delete lt x
  nrt = delete rt x

search :: (Ord a) => BSTree a -> a -> Bool
search Nil _ = False
search (BSTree a lt rt) x | x == a = True
                          | x < a  = search lt x
                          | x > a  = search rt x

initTree :: (Ord a) => [] a -> BSTree a
initTree [] = Nil
initTree xs = foldl insert Nil xs

-- let a = Nil
-- b = insert a 6
-- c = insert b 3
-- d = insert c 4
-- e = insert d 9
-- let x = initTree [6, 2, 3, 1, 7, 9, 1]

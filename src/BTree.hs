module BTree where

data BTree a =
                Nil Int
                | Leaf Int [a]
                | Node Int [a] [BTree a] deriving Show


insert :: (Ord a, Eq a) => BTree a -> a -> BTree a
insert t x = if isFull t then insertNonFull (split t) x
                          else insertNonFull t x

insertNonFull :: (Ord a, Eq a) => Tree a -> a -> Tree a
insertNonFull (Nil m) x = Leaf m [x]
insertNonFull (Leaf m []) x = Leaf m [x]
insertNonFull l@(Leaf m keys@(k:ks)) x
  | x == k = l
  | x < k = Leaf m (x:keys)
  | x > k = Leaf m (x:new_ks)
    where Leaf _ new_ks = insertNonFull (Leaf m ks) x
insertNonFull (Node m [] (t:ts)) x = if isFull t then insertNonFull (split t) x
                                                    else Node m [] [insertNonFull t x]

insertNonFull n@(Node m keys@(k:ks) trees@(t:ts)) x
  | x == k = n
  | x < k = if isFull t then insertNonFull (Node m (newK:k:ks) (newT1:newT2:ts)) x
                         else Node m keys (insertNonFull t x:ts)

  | x > k = Node m (k:new_ks) (t:new_ts)
    where Node _ new_ks new_ts = insertNonFull (Node m ks ts) x
          Node _ [newK] [newT1, newT2] = split t


isFull :: (Ord a, Eq a) => Tree a -> Bool
isFull (Nil _) = False
isFull (Leaf m ks)
  | length ks == (2 * m - 1) = True
  | otherwise = False
isFull (Node m ks _)
  | length ks == (2 * m - 1) = Tree
  | otherwise = False


find :: (Ord a, Eq a) => Tree a -> a -> Bool
find (Nil, _) _ = False
find (Leaf _ []) _ = False
find (Leaf m (k:ks)) x
  | x == k = True
  | x < k = False
  | x > k = find (Leaf m ks) x
find (Node _ [] (t:ts)) x = find t x
find (Node m (k:ks) (t:ts)) x
  | x == k = True
  | x < k = find t x
  | x > k = find (Node m ks ts) x

leftHalf :: [a] -> [a]
leftHalf xs = take (div (length xs) 2) xs

rightHalf :: [a] -> [a]
rightHalf xs = drop (div (length xs) 2) xs


getMin :: (Ord a, Eq a) => Tree a -> a
getMin (Leaf _ keys) = head keys
getMin (Node _ _ trees) = getMin $ head trees


getMax :: (Ord a, Eq a) => Tree a -> a
getMax (Leaf _ keys) = last keys
getMax (Node _ _ trees) = getMax $ last trees


split :: (Ord a, Eq a) => Tree a -> Tree a
split (Leaf m keys) = Node m [k] [Leaf m k1, Node m k2]
  where k1 = leftHalf keys
        k:k2 = rightHalf keys
split (Node m keys trees) = Node m [k] [Node m k1 t1, Node m k2 t2]
  where k1 = leftHalf keys
        k:k2 = rightHalf keys
        t1 = leftHalf trees
        t2 = rightHalf trees

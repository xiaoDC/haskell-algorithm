module BTree where

data BTree a =
                Nil Int
                | Leaf Int [a]
                | Node Int [a] [BTree a] deriving Show


insert :: (Ord a, Eq a) => BTree a -> a -> BTree a
insert t x = if is_full t then insert_non_full (split t) x
                          else insert_non_full t x

insert_non_full :: (Ord a, Eq a) => Tree a -> a -> Tree a
insert_non_full (Nil m) x = Leaf m [x]
insert_non_full (Leaf m []) x = Leaf m [x]
insert_non_full l@(Leaf m keys@(k:ks)) x
  | x == k = l
  | x < k = Leaf m (x:keys)
  | x > k = Leaf m (x:new_ks)
    where Leaf _ new_ks = insert_non_full (Leaf m ks) x
insert_non_full (Node m [] (t:ts)) x = if is_full t then insert_non_full (split t) x
                                                    else Node m [] [(insert_non_full t x)]

insert_non_full n@(Node m keys@(k:ks) trees@(t:ts)) x
  | x == k = n
  | x < k = if is_full t then insert_non_full (Node m (newK:k:ks) (newT1:newT2:ts)) x
                         else Node m keys ((insert_non_full t x):ts)

  | x > k = Node m (k:new_ks) (t:new_ts)
    where Node _ new_ks new_ts = insert_non_full (Node m ks ts) x
          Node _ [newK] [newT1, newT2] = split t


is_full :: (Ord a, Eq a) => Tree a -> Bool
is_full (Nil _) = False
is_full (Leaf m ks)
  | length ks == (2 * m - 1) = True
  | otherwise = False
is_full (Node m ks _)
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

left_half :: [a] -> [a]
left_half xs = take (div (length xs) 2) xs

right_half :: [a] -> [a]
right_half xs = drop (div (length xs) 2) xs


get_min :: (Ord a, Eq a) => Tree a -> a
get_min (Leaf _ keys) = head keys
get_min (Node _ _ trees) = get_min $ head trees


get_max :: (Ord a, Eq a) => Tree a -> a
get_max (Leaf _ keys) = last keys
get_max (Node _ _ trees) = get_max $ last trees


split :: (Ord a, Eq a) => Tree a -> Tree a
split (Leaf m keys) = Node m [k] [Leaf m k1, Node m k2]
  where k1 = left_half keys
        k:k2 = right_half keys
split (Node m keys trees) = Node m [k] [Node m k1 t1, Node m k2 t2]
  where k1 = left_half keys
        k:k2 = right_half keys
        t1 = left_half trees
        t2 = right_half trees

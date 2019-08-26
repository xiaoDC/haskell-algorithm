module Heap where

data Heap a = Empty | Heap a Int (Heap a) (Heap a)


dis :: Heap a -> Int
dis Empty = 0
dis (Heap _ v _ _) = v


val :: (Ord a) => Heap a -> a
val Empty = error "empty heap has no value"
val (Heap v _ _ _) = v


findMin :: (Ord a) => Heap a -> a
findMin Empty = error "Cannot access an empty heap"
findMin (Heap t _ _ _) = t

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge Empty v = v
merge v Empty = v
merge u@(Heap uval udis ulc urc) v@(Heap vval vdis vlc vrc)
  | uval <= vval = let x = merge urc v in
                     if dis url < dis x
                     then Heap uval (dis ulc + 1) x ulc
                     else Heap uval (dis x + 1) ulc x
  | otherwise = merge v u

insert :: (Ord a) => Heap a -> a -> Heap a
insert hp val = merge hp (Heap val 1 Empty Empty)

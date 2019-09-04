module HuffmanTree where

import Data.List (insertBy, sortOn)
import Data.Ord (comparing)

data HTree a = Leaf a | Branch (HTree a) (HTree a) deriving Show


htree [(_, t)] = t
htree ((w1, t1):(w2, t2):xs) = htree $ insertBy (comparing fst) (w1+w2, Branch t1 t2) xs


serialize (Leaf x) = [(x, "")]
serialize (Branch l r) =
  [(x, '0':code) | (x, code) <- serialize l] ++
  [(x, '1':code) | (x, code) <- serialize r]


huffman :: (Ord a, Ord w, Num w) => [(a, w)] -> [(a, String)]
huffman freq =
  sortOn fst $
  serialize $ htree $ sortOn fst [(w, Leaf x) | (x, w) <- freq]

-- huffman (zip ['a'..] [0.4, 0.3, 0.1, 0.1, 0.06, 0.04])

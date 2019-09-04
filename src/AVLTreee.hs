module AVLTreee where


data AVLTreee a = Empty | AVLTreee a (AVLTreee a) (AVLTreee a) deriving (Show, Eq)


balance :: (Ord a) => AVLTreee a -> AVLTreee a -> Int
balance a b = height a - height b

value (AVLTreee v _ _) = v


insert :: (Ord a) => AVLTreee a -> a -> AVLTreee a
insert Empty a = AVLTreee a Empty Empty
insert (AVLTreee v lt rt) x
  | x < v && balance lti rt == 2 && x < value lt = rebalanceLL (AVLTreee v lti rt)
  | x < v && balance lti rt == 2 && x > value lt = rebalanceLR (AVLTreee v lti rt)
  | x > v && balance rti lt == 2 && x < value rt = rebalanceRL (AVLTreee v lt rti)
  | x > v && balance rti lt == 2 && x > value rt = rebalanceRR (AVLTreee v lt rti)
  | x < v = AVLTreee v lti rt
  | x > v = AVLTreee v lt rti
 where
  lti = insert lt x
  rti = insert rt x


height :: (Ord a) => AVLTreee a -> Int
height Empty = 0
height (AVLTreee _ lt rt) | lh < rh   = rh + 1
                          | otherwise = lh + 1
 where
  lh = height lt
  rh = height rt


rebalanceLL (AVLTreee v (AVLTreee vl tll tlr) tr) =
  AVLTreee vl tll (AVLTreee v tlr tr)
rebalanceLR (AVLTreee v (AVLTreee vl tll (AVLTreee vlr tlrl tlrr)) tr) =
  AVLTreee vlr (AVLTreee vl tll tlrl) (AVLTreee v tlrr tr)
rebalanceRL (AVLTreee v tl (AVLTreee vr (AVLTreee vrl trll trlr) trr)) =
  AVLTreee vrl (AVLTreee v tl trll) (AVLTreee vr trlr trr)
rebalanceRR (AVLTreee v tl (AVLTreee vr trl (AVLTreee vrr trrl trrr))) =
  AVLTreee vr (AVLTreee v tl trl) (AVLTreee vrr trrl trrr)


delete :: AVLTreee a -> Int -> AVLTreee a
delete Empty _ = Empty
delete r@(AVLTreee v Empty Empty) d | v == d    = Empty
                                    | otherwise = r
-- delete (AVLTreee v )



-- let a = insert Empty 5
-- let b = insert a 3
-- let c = insert b 2
-- let d = insert c 6
-- let e = insert d 7
-- let f = insert e 4

module EightQueen where

noSameRow []       = True
noSameRow (x : xs) = notElem x xs && noSameRow xs


noSameDiag []           = True
noSameDiag xs@(x : xs') = and [ abs (i1 - i) /= abs (p1 - p) | (i, p) <- ip ]
  && noSameDiag xs'
  where (i1, p1) : ip = zip [1 ..] xs


positions 0 n = [[]]
positions k n =
  [ p : ps | ps <- positions (k - 1) n, p <- [1 .. n], isSafe p ps ]


isSafe p ps = not (p `elem` ps || sameDiag p ps)
  where sameDiag p ps = any (\(dist, q) -> abs (p - q) == dist) $ zip [1 ..] ps

queen n = positions n n

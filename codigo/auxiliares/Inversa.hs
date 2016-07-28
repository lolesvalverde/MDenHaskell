import Data.List (nub)

inversa :: (Eq a, Eq b) => [(a,b)] -> [(b,a)]
inversa [] = []           
inversa xs@((a,b):fs) = nub (inversaAux xs)
      where inversaAux []         = []            
            inversaAux ((x,y):ys) = (y,x):inversaAux ys 

inversa2 :: (Eq a, Eq b) => [(a,b)] -> [(b,a)]
inversa2 f = [(y,x) | (x,y) <- f]

-- λ> length (inversa [(x,x+1) | x <- [1..10^5]])
-- 100000
-- (168.22 secs, 37,586,864 bytes)
-- λ> length (inversa2 [(x,x+1) | x <- [1..10^5]])
-- 100000
-- (0.07 secs, 33,880,656 bytes)

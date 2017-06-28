-- Informatics 1 - Functional Programming 
-- Tutorial 7
--
-- Week 9 - Due: 20/21 Nov.


import LSystem
import Test.QuickCheck

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split (p :#: Sit)  = [p] 
split (Sit :#: q)  = split q
split (p :#: q)    = split p ++ split q 
split  p           = [p] 

-- 1b. join
join :: [Command] -> Command
join [c] = c 
join (c:cs) = c :#: join cs

--join comms = foldr (:#:) Sit comms


--1c. testing join, split
prop_join_split :: Command -> Bool
prop_join_split c = split (join (split c)) == split c



-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy i com = join [ com | c <- [1..i]] 

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon i = copy 5 (Go i :#: Turn 72) 

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon d s = copy s (Go d :#: Turn a)
  where a = 360 / fromIntegral s


-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral 0 _ _ _ = Sit
spiral _ 0 _ _ = Sit
spiral l n i a = (Go l :#: Turn a) :#: spiral (l+i) (n-1) i a


-- Exercise 4
-- optimise
{-
rotate :: [a] -> Int -> [a]
rotate [] i = []
rotate x 0 = x 
rotate (x:xs) i = rotate (i-1) (xs ++ [x]) 

optimise :: Command -> Command 
optimise com = join (f1 (check (coms com))
  where f1 c = filter (\x -> (x \= Sit) && (x \= Turn 0) && ( x \= Go 0) (split c)
        coms c = zip (f1 c) (rotate (f1 c) 1)
        check (com:coms) = 
         | fst com == snd com    ... -} 
{-                  
optimise (Sit :#: (q :#: r)) = optimise q
optimise (Turn 0 :#:(q :#: r)) = optimise q
optimise (Go 0 :#: (q :#: r)) = optimise q

optimise (Turn p :#: (Turn q :#: r))  = optimise (Turn (p+q) :#: r)
optimise (Go p :#: (Go q :#: r))     = optimise (Go   (p+q) :#: r)

optimise (p :#: Turn 0) = optimise p
optimise (p :#: Go 0) = optimise p
optimise (p :#: Sit) = optimise p

optimise p = p
-}
-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x = f x
 where
  f 0  = GrabPen red :#: Go 10  
  f (x+1) = g x :#: p :#: f x :#: p :#: g x
  g 0 = GrabPen blue :#: Go 10
  g (x+1) = f x :#: n :#: g x :#: n :#: f x
  n = Turn 60
  p = Turn (-60)

-- 6. snowflake
snowflake :: Int -> Command
snowflake x = f x 
 where 
  f 0  = GrabPen red :#: copy 3 (Go 10 :#: n :#: n)
  f (x+1) = f x :#: p :#: f x :#: n :#: n :#: f x :#: p :#: f x  
  n = Turn 60
  p = Turn (-60)

-- 7. hilbert
{- hilbert :: Int -> Command
hilbert x = f x
 where
  f 0  = GrabPen red :#: Go 10  
  f (x+1) = g x :#: p :#: f x :#: p :#: g x
  g 0 = GrabPen blue :#: Go 10
  g (x+1) = f x :#: n :#: g x :#: n :#: f x
  n = Turn 90
  p = Turn (-90)
-}
-- Informatics 1 - Functional Programming 
-- Tutorial 7
--
-- Solutions
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!


import LSystem
import Test.QuickCheck


-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split Sit             =  []
split (cmd1 :#: cmd2) =  split cmd1 ++ split cmd2
split cmd             =  [cmd]

-- 1b. join
join :: [Command] -> Command
join  =  foldr (:#:) Sit

-- alternative join (using foldl)
join' :: [Command] -> Command
join' [] = Sit
join' (x:xs) = foldl (:#:) x xs

--1c. equivalence testing
prop_split_join :: Command -> Bool
prop_split_join cmd  =  split (join (split cmd)) == split cmd 



-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy n cmd  |  n <= 0     =  Sit
            |  n == 1     =  cmd
            |  otherwise  =  cmd :#: copy (n-1) cmd

-- alternative copy (using join, replicate)
copy' :: Int -> Command -> Command
copy' n cmd = join (replicate n cmd)

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon side = copy 5 (Go side :#: Turn 72.0)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon side nsides = 
    copy nsides (Go side :#: Turn angle)
        where angle = 360 / (fromIntegral nsides)



-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral side n step angle  |  side <= 0 || n == 0  =  Sit 
                          |  otherwise   =
    Go side :#: Turn angle :#: (spiral (side+step) (n-1) step angle)

-- alternative using comprehension
spiral' side n step angle  = join
  [ Go (side + i * step) :#: Turn angle | i <- [0..n], (side + i * step) > 0 ]



-- Exercise 4
-- optimise
optimise :: Command -> Command
optimise cmd = join' $ optimiseLoop $ split cmd
  where 
    optimiseLoop x  =  if x' == x then x else optimiseLoop x'
        where x'    =  optimiseStep x
    optimiseStep [] = []
    optimiseStep (Sit : path)             = optimiseStep path
    optimiseStep (Go 0 : path)            = optimiseStep path
    optimiseStep (Go x : Go y : path)     = optimiseStep (Go(x+y) : path)
    optimiseStep (Turn 0 : path)          = optimiseStep path
    optimiseStep (Turn x : Turn y : path) = optimiseStep (Turn(x+y) : path)
    optimiseStep (cmd : path)             = (cmd : optimiseStep path)



-- L-Systems

-- 5. arrowhead
arrowhead x = f x
    where
      f 0 = GrabPen red :#: Go 10
      f (x+1) = g x :#: n :#: f x :#: n :#: g x
      g 0 = GrabPen blue :#: Go 10
      g (x+1) = f x :#: p :#: g x :#: p :#: f x
      n = Turn 60
      p = Turn(-60)


-- 6. snowflake
snowflake x = f x :#: n :#: n :#: f x :#: n :#: n :#: f x :#: n :#: n 
    where
      f 0 = Go 10
      f (x+1) = f x :#: p :#: f x :#: n :#: n :#: f x :#: p :#: f x
      n = Turn 60
      p = Turn(-60)


-- 7. hilbert
hilbert x = l x
    where
      l 0 = Sit 
      l (x+1) = n :#: r x :#: f :#: p :#: l x :#: f :#: l x :#: p :#: f :#: r x :#: n
      r 0 = Sit 
      r (x+1) = p :#: l x :#: f :#: n :#: r x :#: f :#: r x :#: n :#: f :#: l x :#: p
      f = GrabPen black :#: Go 10
      n = Turn 90
      p = Turn(-90)



-- Bonus L-Systems

peanoGosper x = f x
    where 
      f (x+1) = f x :#: n :#: g x :#: n :#: n :#: g x :#: p :#: f x :#: p :#: p :#: f x :#: f x :#: p :#: g x :#: n
      f 0 = GrabPen red :#: Go 10
      g (x+1) = p :#: f x :#: n :#: g x :#: g x :#: n :#: n :#: g x :#: n :#: f x :#: p :#: p :#: f x :#: p :#: g x
      g 0 = GrabPen blue :#: Go 10
      n = Turn 60
      p = Turn(-60)


cross x = f x :#: n :#: f x :#: n :#: f x :#: n :#: f x
    where
      f 0 =  Go 10
      f (x+1) = f x :#: n :#: f x :#: p :#: f x :#: p :#: f x :#: f x :#: n :#: f x :#: n :#: f x :#: p :#: f x 
      n = Turn 90
      p = Turn(-90)


branch x = g x
   where
     g 0 = GrabPen red :#: Go 10
     g (x+1) = f x :#: p :#: Branch (Branch (g x) :#: n :#: g x) :#: f x :#: Branch (n :#: f x :#: g x) :#: p :#: g x
     f 0 = GrabPen blue :#: Go 10
     f (x+1) = f x :#: f x
     n = Turn 22.5
     p = Turn(-22.5)


thirtytwo x = f x :#: n :#: f x :#: n :#: f x :#: n :#: f x
    where
      f 0 = Go 10.0
      f (x+1) =  p :#: f x :#: n :#: f x :#: p :#: f x :#: p :#: f x :#: n :#: f x :#: n :#: f x :#: f x :#: p :#: f x :#: n :#: f x :#: n :#: f x :#: f x :#: n :#: f x :#: p :#: f x :#: p :#: f x :#: f x :#: n :#: f x :#: f x :#: p :#: f x :#: f x :#: n :#: f x :#: n :#: f x :#: p :#: f x :#: f x :#: p :#: f x :#: p :#: f x :#: n :#: f x :#: f x :#: p :#: f x :#: p :#: f x :#: n :#: f x :#: n :#: f x :#: p :#: f x :#: n
      n = Turn 90
      p = Turn (-90)

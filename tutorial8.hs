-- Informatics 1 - Functional Programming 
-- Tutorial 8
--
-- Week 8 - due: 27/28 Nov.

import List
import Test.QuickCheck



-- Type declarations

type FSM q = ([q], Alphabet, q, [q], [Transition q])
type Alphabet = [Char]
type Transition q = (q, Char, q)



-- Example machines

m1 :: FSM Int
m1 = ([0,1,2,3,4],
      ['a','b'],
      0,
      [4],
      [(0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2),
       (1,'b',4), (2,'a',3), (2,'b',3), (3,'b',4),
       (4,'a',4), (4,'b',4)])

m2 :: FSM Char
m2 = (['A','B','C','D'],
      ['0','1'],
      'B',
      ['A','B','C'],
      [('A', '0', 'D'), ('A', '1', 'B'),
       ('B', '0', 'A'), ('B', '1', 'C'),
       ('C', '0', 'B'), ('C', '1', 'D'),
       ('D', '0', 'D'), ('D', '1', 'D')])

dm1 :: FSM [Int] 
dm1 =  ([[],[0],[1,2],[3],[3,4],[4]],
        ['a','b'],
        [0],
        [[3,4],[4]],
        [([],   'a',[]),
         ([],   'b',[]),
         ([0],  'a',[1,2]),
         ([0],  'b',[1,2]),
         ([1,2],'a',[3]),
         ([1,2],'b',[3,4]),
         ([3],  'a',[]),
         ([3],  'b',[4]),
         ([3,4],'a',[4]),
         ([3,4],'b',[4]),
         ([4],  'a',[4]),
         ([4],  'b',[4])])



-- 1.
states :: FSM q -> [q]
states (u,a,s,f,t) = u

alph   :: FSM q -> Alphabet
alph (u,a,s,f,t) = a

start  :: FSM q -> q
start (u,a,s,f,t) = s

final  :: FSM q -> [q]
final (u,a,s,f,t) = f

trans  :: FSM q -> [Transition q]
trans (u,a,s,f,t) = t

-- 2.
delta :: (Eq q) => FSM q -> q -> Char -> [q]
delta fsm p c = [ q | (p',i,q) <- trans fsm, (p' == p) && (i == c) ] 


-- 3.
accepts :: (Eq q) => FSM q -> String -> Bool
accepts fsm cs = acceptsFrom fsm (start fsm) cs

acceptsFrom :: (Eq q) => FSM q -> q -> String -> Bool
acceptsFrom fsm q []     = q `elem` final fsm
acceptsFrom fsm q (c:cs) = delta fsm q c /= [] && or [acceptsFrom fsm p cs | p <- delta fsm q c]

-- 4.
canonical :: (Ord q) => [q] -> [q]
canonical qs = List.sort (List.nub qs)


-- 5.
ddelta :: (Ord q) => FSM q -> [q] -> Char -> [q]
ddelta fsm qs c = canonical (concat [delta fsm q c | q <- qs ]) 


-- 6.
next :: (Ord q) => FSM q -> [[q]] -> [[q]]
next fsm qss = canonical (qss ++ [ddelta fsm qs c | qs <- qss, c <- alph fsm])


-- 7.
reachable :: (Ord q) => FSM q -> [[q]] -> [[q]]
reachable fsm qss 
   | n == next fsm n = n
   | otherwise       = reachable fsm n
 where n = next fsm qss    


-- 8.
dfinal :: (Ord q) => FSM q -> [[q]] -> [[q]]
dfinal fsm qss = [ q | q <- qss, containsFinal fsm q]

containsFinal :: (Ord q) => FSM q -> [q] -> Bool
containsFinal fsm qss = or [f `elem` qss | f <- final fsm ] 


-- 9.
dtrans :: (Ord q) => FSM q -> [[q]] -> [Transition [q]]
dtrans fsm pss = [ (p,a,ddelta fsm p a) | p <- pss, a <- alph fsm ]


-- 10.

 -- error:  Occurs check: cannot construct the infinite type: q = [q]
 -- When generalising the type(s) for `deterministic'

deterministic :: (Ord q) => FSM q -> FSM [q]
deterministic fsm = ( d,
                      alph fsm,
                      start fsm,
                      dfinal fsm d,
                      dtrans fsm d)  
  where  d = reachable fsm [[start fsm]]

-- 11.
prop_deterministic :: (Ord q) => FSM q -> [Int] -> Bool
prop_deterministic = undefined

makeInput :: Alphabet -> [Int] -> String
makeInput [] _     =  error "makeInput: empty alphabet"
makeInput alph xs  =  map f xs
    where f x  =  alph !! (x `mod` length alph)

-- Propositions, v4   -  solutions version
-- Philip Wadler, 4 Nov 2004
-- (infixified constructors - Jeremy Yallop, 11. Nov 2005)
-- (changes - Willem Heijltjes, Oct 2008:
--  * Merged 'Table' and 'Prop' modules with the exercises.
--  * Added QuickCheck Arbitrary instances.
--  * Used 'nub (xs ++ ys)' instead of 'union' in 'names' function.
--  * Used 'lookUp' (see bottom) instead of library code for lookup.)


-- Informatics 1 - Functional Programming 
-- Tutorial 6
--
-- Solutions
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!


import Control.Monad( liftM, liftM2 )
import Data.List( nub )
import Test.QuickCheck


-- The datatype 'Prop'

type Name = String
data Prop = Var Name
          | F
          | T
          | Not Prop
          | Prop :|: Prop
          | Prop :&: Prop
          | Prop :->: Prop
          | Prop :<->: Prop
          deriving (Eq, Ord)

type Names = [Name]
type Env = [(Name, Bool)]


-- Functions for handling Props

-- turns a Prop into a string approximating mathematical notation
showProp :: Prop -> String
showProp (Var x)        =  x
showProp (F)            =  "F"
showProp (T)            =  "T"
showProp (Not p)        =  "(~" ++ showProp p ++ ")"
showProp (p :|: q)      =  "(" ++ showProp p ++ "|" ++ showProp q ++ ")"
showProp (p :&: q)      =  "(" ++ showProp p ++ "&" ++ showProp q ++ ")"
showProp (p :->: q)     =  "(" ++ showProp p ++ "->" ++ showProp q ++ ")"
showProp (p :<->: q)    =  "(" ++ showProp p ++ "<->" ++ showProp q ++ ")"

-- evaluates a proposition in a given environment
eval :: Env -> Prop -> Bool
eval e (Var x)        =  lookUp x e
eval e (F)            =  False
eval e (T)            =  True
eval e (Not p)        =  not (eval e p)
eval e (p :|: q)      =  eval e p || eval e q
eval e (p :&: q)      =  eval e p && eval e q
eval e (p :->: q)     =  not (eval e p) || eval e q
eval e (p :<->: q)    =  eval e p == eval e q

-- retrieves the names of variables from a proposition - 
--  NOTE: variable names in the result must be unique
names :: Prop -> Names
names (Var x)        =  [x]
names (F)            =  []
names (T)            =  []
names (Not p)        =  names p
names (p :|: q)      =  nub (names p ++ names q)
names (p :&: q)      =  nub (names p ++ names q)
names (p :->: q)     =  nub (names p ++ names q)
names (p :<->: q)    =  nub (names p ++ names q)

-- creates all possible truth assignments for a set of variables
envs :: Names -> [Env]
envs []      =  [[]]
envs (x:xs)  =  [ (x,False):e | e <- envs xs ] ++
                [ (x,True ):e | e <- envs xs ]

-- checks whether a proposition is satisfiable
satisfiable :: Prop -> Bool
satisfiable p  =  or [ eval e p | e <- envs (names p) ]


-- 1.
p1  =  (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")
p2  =  (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))
p3  =  (Var "P" :&: (Var "Q" :|: Var "R")) :&: 
       ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not (Var "R")))


-- 2. 
tautology :: Prop -> Bool
tautology p  =  and [ eval e p | e <- envs (names p) ]

prop_taut1 :: Prop -> Bool
prop_taut1 p  =  tautology p || satisfiable (Not p)

prop_taut2 :: Prop -> Bool
prop_taut2 p  =  not (satisfiable p) || not (tautology (Not p))


-- 3.
p4  =  (Var "P" :->: Var "Q") :&: (Var "P" :<->: Var "Q")
p5  =  (Var "P" :->: Var "Q") :&: (Var "P" :&: Not (Var "Q"))
p6  =  (Var "P" :<->: Var "Q") :&: ((Var "P" :&: Not (Var "Q")) :|: (Not (Var "P") :&: Var "Q"))


-- 4.
equivalent :: Prop -> Prop -> Bool
equivalent p q  =  and [eval e p == eval e q | e <- theEnvs]
    where
      theNames  =  nub (names p ++ names q)
      theEnvs   =  envs theNames

equivalent' :: Prop -> Prop -> Bool
equivalent' p q  =  tautology (p :<->: q)

prop_equivalent :: Prop -> Prop -> Bool
prop_equivalent p q  =  equivalent p q == equivalent' p q



-- 5.
subformulas :: Prop -> [Prop]
subformulas prop  = prop : nub (f prop)
    where
      f (Not p)      = subformulas p
      f (p :|: q)    = subformulas p ++ subformulas q
      f (p :&: q)    = subformulas p ++ subformulas q
      f (p :->: q)   = subformulas p ++ subformulas q
      f (p :<->: q)  = subformulas p ++ subformulas q
      f _            = []



-- Optional exercises

-- 6. 
-- convert to negation normal form
toNNF :: Prop -> Prop
toNNF (Not (p :|: q))    =  toNNF (Not p) :&: toNNF (Not q)
toNNF (Not (p :&: q))    =  toNNF (Not p) :|: toNNF (Not q)
toNNF (Not (p :->: q))   =  toNNF p :&: toNNF (Not q)
toNNF (Not (p :<->: q))  =  (toNNF (Not p) :&: toNNF q) :|: (toNNF p :&: toNNF (Not q))
toNNF (Not (Not p))      =  toNNF p
toNNF (Not (Var a))      =  Not (Var a)
toNNF (Not T)            =  F
toNNF (Not F)            =  T
toNNF (p :|: q)          =  toNNF p :|: toNNF q
toNNF (p :&: q)          =  toNNF p :&: toNNF q
toNNF (p :->: q)         =  toNNF (Not p) :|: toNNF q
toNNF (p :<->: q)        =  (toNNF (Not p) :&: toNNF (Not q)) :|: (toNNF p :&: toNNF q)
toNNF p                  =  p

-- check for negation normal form
isNNF :: Prop -> Bool
isNNF (p :&: q)      =  isNNF p && isNNF q
isNNF (p :|: q)      =  isNNF p && isNNF q
isNNF (Not (Var _))  =  True
isNNF (Var _)        =  True
isNNF T              =  True
isNNF F              =  True
isNNF _              =  False

-- check if result of toNNF is in neg. normal form
prop_NNF1 :: Prop -> Bool
prop_NNF1 p  =  isNNF (toNNF p)

-- check if result of toNNF is equivalent to its input
prop_NNF2 :: Prop -> Bool
prop_NNF2 p  =  equivalent p (toNNF p)


-- 7.
-- convert to conjunctive normal form
toCNF :: Prop -> Prop
toCNF p  =  listsToCNF (toCNFL p)

-- convert to (conjunct) list of (disjunct) lists
toCNFL :: Prop -> [[Prop]]
toCNFL p = c (toNNF p) 
    where
      c F              =  [[]]
      c T              =  []
      c (Var n)        =  [[Var n]]
      c (Not (Var n))  =  [[Not (Var n)]]
      c (p :&: q)      =  (c p ++ c q)
      c (p :|: q)      =  [x ++ y | x <- c p, y <- c q]

-- check if result of toCNF is equivalent to its input
prop_CNF :: Prop -> Bool
prop_CNF p  =  equivalent p (toCNF p)

-- check if result of toCNFL is equivalent to its input
prop_CNFL :: Prop -> Bool
prop_CNFL p  =  equivalent p (listsToCNF (toCNFL p))

-- check whether a formula is in conj. normal form
isCNF :: Prop -> Bool
isCNF T          =  True
isCNF F          =  True
isCNF (p :&: q)  =  isCNF p && isCNF q
isCNF p          =  isClause p
    where 
      isClause (p :|: q)  =  isClause p && isClause q
      isClause p          =  isLiteral p
          where
            isLiteral (Not (Var _))  =  True
            isLiteral (Var _)        =  True
            isLiteral _              =  False

-- transform a list of lists into a (CNF) formula
listsToCNF :: [[Prop]] -> Prop
listsToCNF xss  |  null xss      =  T
                |  any null xss  =  F
                |  otherwise     =  (foldl1 (:&:) . map (foldl1 (:|:))) xss

-- transform a CNF formula into a list of lists
listsFromCNF :: Prop -> [[Prop]]
listsFromCNF p  |  not (isCNF p)  =  error "listsFromCNF: formula is not in CNF"
                |  otherwise      =  getAnds p
                where
                  getAnds (p :&: q)  =  getAnds p ++ getAnds q
                  getAnds p          =  [getOrs p]
                  getOrs (p :|: q)   =  getOrs p ++ getOrs q
                  getOrs p           =  [p]


-- For QuickCheck

instance Show Prop where
    show  =  showProp

instance Arbitrary Prop where
    arbitrary  =  sized prop
        where
          prop n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM Not subform
                                       , liftM2 (:|:) subform subform
                                       , liftM2 (:&:) subform subform
                                       , liftM2 (:->:) subform subform
                                       , liftM2 (:<->:) subform' subform'
                                       ]
                 where
                   atom  =  oneof [liftM Var (elements ["P", "Q", "R", "S"]),
                                   elements [F,T]]
                   subform  =  prop (n `div` 2)
                   subform' =  prop (n `div` 4)
    coarbitrary a  =  undefined


-- Drawing Tables:

-- centre a string in a field of a given width
centre :: Int -> String -> String
centre w s  =  replicate h ' ' ++ s ++ replicate (w-n-h) ' '
            where
            n = length s
            h = (w - n) `div` 2

-- make a string of dashes as long as the given string
dash :: String -> String
dash s  =  replicate (length s) '-'

-- convert boolean to T or F
fort :: Bool -> String
fort False  =  "F"
fort True   =  "T"

-- print a table with columns neatly centred
-- assumes that strings in first row are longer than any others
showTable :: [[String]] -> IO ()
showTable tab  =  putStrLn (
  unlines [ unwords (zipWith centre widths row) | row <- tab ] )
    where
      widths  = map length (head tab)

table p = tables [p]

tables :: [Prop] -> IO ()
tables ps  =
  let xs = nub (concatMap names ps) in
    showTable (
      [ xs            ++ ["|"] ++ [showProp p | p <- ps]           ] ++
      [ dashvars xs   ++ ["|"] ++ [dash (showProp p) | p <- ps ]   ] ++
      [ evalvars e xs ++ ["|"] ++ [fort (eval e p) | p <- ps ] | e <- envs xs]
    )
    where  dashvars xs        =  [ dash x | x <- xs ]
           evalvars e xs      =  [ fort (eval e (Var x)) | x <- xs ]

-- print a truth table, including columns for subformulas
fullTable :: Prop -> IO ()
fullTable = tables . filter nontrivial . subformulas
    where nontrivial :: Prop -> Bool
          nontrivial (Var _) = False
          nontrivial T       = False
          nontrivial F       = False
          nontrivial _       = True


-- Auxiliary functions

lookUp :: Eq a => a -> [(a,b)] -> b
lookUp z xys  =  the [ y | (x,y) <- xys, x == z ]
    where the [x]  =  x
          the _    =  error "eval: lookUp: variable missing or not unique"

-- Informatics 1 - Functional Programming 
-- Tutorial 2
-- Pierre Pauly
-- Week 4 - due: 16/17 Oct.

import Data.Char
import Data.List
import Test.QuickCheck



-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]

-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs)
  | (x `mod` 2 == 0) =  x `div` 2 : halveEvensRec xs 
  | otherwise      =  halveEvensRec xs  

-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensRec xs



-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange f l domain = [x | x <- domain, f <= x && x <= l] 

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec _ _ [] = []
inRangeRec f l (x:xs) 
  | f <= x && x <= l    = x : inRangeRec f l xs
  | otherwise           =     inRangeRec f l xs

-- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRange lo hi xs == inRangeRec lo hi xs



-- 3. sumPositives: sum up all the positive numbers in a list

-- List-comprehension version
sumPositives :: [Int] -> Int
sumPositives xs = sum [x | x <- xs, x > 0] 

-- Recursive version
sumPositivesRec :: [Int] -> Int
sumPositivesRec [] = 0
sumPositivesRec (x:xs)
 | x > 0     =  x + sumPositivesRec xs
 | otherwise = sumPositivesRec xs

-- Mutual test
prop_sumPositives :: [Int] -> Bool
prop_sumPositives xs = sumPositives xs == sumPositivesRec xs 



-- 4. pennypincher

-- List-comprehension version.
pennypincher :: [Float] -> Float
pennypincher xs = sum [x - (x / 10) | x <- xs, (x - (x/10)) <= 199] 

-- Recursive version
pennypincherRec :: [Float] -> Float
pennypincherRec [] = 0
pennypincherRec (x:xs) 
 | (x - x/10) <= 199   = (x - x/10) + pennypincherRec xs  
 | (x - x/10) >  199   = pennypincherRec xs 

-- For this one, we don't do a mutual test, because it would fail 
-- on rounding errors



-- 5. sumDigits

-- List-comprehension version
sumDigits :: String -> Int
sumDigits str = sum [ (ord x) - (ord '0') | x <- str, isDigit x ] 

-- Recursive version
sumDigitsRec :: String -> Int
sumDigitsRec [] = 0
sumDigitsRec (x:xs)
 | isDigit x   = (ord x - ord '0') + sumDigitsRec xs
 | otherwise   = sumDigitsRec xs  
     

-- Mutual test
prop_sumDigits :: String -> Bool
prop_sumDigits str = sumDigits str == sumDigitsRec str



-- 6. capitalized

-- List-comprehension version
capitalized :: String -> String 
capitalized (x:xs) = toUpper x : [ toLower y | y <- (xs) ] 

-- Recursive version
capitalizedRec :: String -> String
capitalizedRec (x:xs) = toUpper x : lowerRest xs  

lowerRest :: String -> String
lowerRest [] = []
lowerRest (x:xs) = toLower x : lowerRest xs

-- Mutual test
prop_capitalized :: String -> Bool
prop_capitalized str = capitalized str == capitalizedRec str



-- 7. title

-- List-comprehension version

strToLower :: String -> String
strToLower str = [ toLower s | s <- str ]

title :: [String] -> [String]
title [] = []
title (x:xs) = capitalized x : [convertOther str | str <- xs]

convertOther :: String -> String
convertOther str 
 | length str < 4  = strToLower str
 | length str >= 4 = capitalized str

-- Recursive version
titleRec :: [String] -> [String]
titleRec [] = []
titleRec (x:xs) = capitalized x : convertRec xs

convertRec :: [String] -> [String]
convertRec [] = []
convertRec (x:xs)  
 | length x >= 4  = capitalized x : convertRec xs
 | length x < 4   = strToLower x : convertRec xs

-- mutual test
prop_title :: [String] -> Bool
prop_title str = title str == titleRec str



-- 8. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind k i l wordlist = [x | x <- (filterLength l wordlist), compareIndices k i x ]

filterLength :: Int -> [String] -> [String]
filterLength len list = [ x | x <- list, length x == len ]

compareIndices :: Char -> Int -> String -> Bool
compareIndices k i str = [ x | x <- findIndices (==k) str, x == i ] /= []


-- Recursive version
crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordFindRec _ _ _ [] = []
crosswordFindRec k i len (x:xs)
 | (length x == len) && (compareIndices k i x)   = x : crosswordFindRec k i len xs
 | otherwise                                     =     crosswordFindRec k i len xs  

-- Mutual test
prop_crosswordFind :: Char -> Int -> Int -> [String] -> Bool
prop_crosswordFind k i len list = crosswordFind k i len list == crosswordFindRec k i len list 



-- 9. search

-- List-comprehension version
search :: String -> Char -> [Int]
search str c = [ y | (x,y) <- (zip str [0..]), x == c]

-- Recursive version                        -- compiles but does not work: error message:
searchRec :: String -> Char -> [Int]        -- Irrefutable pattern failed for pattern [(s, y)] 
searchRec [] _ = []
searchRec (x:xs) c 
 | s == c      = y : searchRec xs c
 | otherwise   =     searchRec xs c 
  where [(s,y)] = zip (x:xs) [0..] 

-- Mutual test
prop_search :: String -> Char -> Bool
prop_search str c = search str c == searchRec str c



-- 10. contains

{-
containsRec :: String -> String -> Bool      
containsRec (x:xs) (y:ys) 
 | isPrefixOf (y:ys) (x:xs) = True
 | otherwise                = or [isPrefixOf (y:ys) xs]
 -}

contains :: String -> String -> Bool
contains long short = or [isPrefixOf short (drop i long) | i <- [0..(length long)-1]]
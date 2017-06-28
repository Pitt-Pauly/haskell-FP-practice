-- Informatics 1 - Functional Programming 
-- Tutorial 3
-- Pierre Pauly
-- Week 5 - due: 23/24 Oct.

import Data.Char
import Data.List
import Test.QuickCheck


-- 1
rotate :: Int -> [Char] -> [Char]
rotate i cs = drop i cs ++ take i cs


-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m | l == 0 = 0
                                | otherwise = k `mod` l

-- 3. 
makeKey :: Int -> [(Char, Char)]
makeKey i = [(x,y) | (x,y) <- zip ['A'..'Z'] (rotate i ['A'..'Z'])]   

-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp c tup_cs 
  | y /= []    = head y
  | otherwise  = c
 where y = [y | (x,y) <- tup_cs, x == c ]  
                   

-- 5.
encipher :: Int -> Char -> Char
encipher i c 
  | isDigit c   = c
  | otherwise   = head [ y | (x,y) <- makeKey i, x == c] 

-- 6.
normalize :: String -> String
normalize [] = []
normalize (x:xs)
  | 'a' <= x && x <= 'z'       = toUpper x : normalize xs
  | ('A' <= x && x <= 'Z') || ('0' <= x && x <= '9')    = x : normalize xs
  | otherwise                  = normalize xs 

-- 7.
encipherStr :: Int -> String -> String
encipherStr i str = [encipher i c | c <- normalize str] 

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey key = [ (y,x) | (x,y) <- key]            

-- 9.
decipher :: Int -> Char -> Char
decipher i c
  | isDigit c  = c
  | otherwise  = head [ y | (x,y) <- reverseKey (makeKey i), x == c ] 

decipherStr :: Int -> String -> String
decipherStr i str = [ decipher i c | c <- str ] 

-- 10.
prop_cipher :: Int -> String -> Property
prop_cipher i str = ns == encipherStr i (decipherStr i ns) 
                ==> ns == decipherStr i (encipherStr i ns)
  where ns = normalize str

-- 11.
candidates :: String -> [(Int, String)]
candidates str = [ (k,s) | 
                   (k,s) <- zip [0..25] deciStr, 
                    contains s "AND" || contains s "THE"]
  where deciStr = [ decipherStr i str | i <- [0..25] ]
      

contains :: String -> String -> Bool
contains long short = or [isPrefixOf short (drop i long) | i <- [0..(length long)-1]]
-- Informatics 1 - Functional Programming
-- Lab Week Exercise
--
-- Week 2 - due: Friday, Oct. 3, 5pm
--
-- Insert your name and matriculation number here:
-- Name: Pierre Pauly
-- Nr. : 0836497


import Test.QuickCheck

{- Exercise 1: 
   (a) 7
   (b) 23 and 35. Yes it does.

 - Exercise 2:
   it's :quit
-}


-- Exercise 3:

double :: Int -> Int
double x = x + x

{- (b) i. 42
      ii. Int
     iii. Int
   (c) It shows an error message saying that "three" is not of the expected type (Int) but a Char, so the 
       function 'double' can't use it.
-}

square :: Int -> Int
square x = x*x

-- Exercise 4:

isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = square(a) + square(b) == square(c)


-- Exercise 5:

leg1 :: Int -> Int -> Int
leg1 x y = square(x)-square(y)

leg2 :: Int -> Int -> Int
leg2 x y = 2*y*x

hyp :: Int -> Int -> Int
hyp x y = square(x)+square(y)


-- Exercise 6:

prop_triple :: Int -> Int -> Bool
prop_triple x y = isTriple (leg1 x y) (leg2 x y) (hyp x y)

{- Exercise 6: 
   (a) prop_triple gets 2 integers as arguments, and uses them with 4 other functions: isTriple, leg1, leg2 and hyp.
       The results of leg1, leg2 and hyp are integers which are then used by isTriple to check if they are a Pythagorean triples.
       The result of prop_triple is a boolean, which is true when isTriple's result is 'true'.(so when all three numbers are Pythagorean triples)
   (b) The functions should work for all positive integers x and y with x<y, since we know that the formular for creating Pythagorean triples works with these 
       numbers. So in that case prop_triple would answer with the boolean true. For the rest, it could be that prop_triple gives true back, but it is not logical
       to use our function for negative integers, since a side of a triangle can't have a negative length. Which means that our functions works fine for the
       normal and logical use. 
-}
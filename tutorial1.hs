-- Informatics 1 - Functional Programming 
-- Tutorial 1
-- Pierre Pauly
-- Week 3 - due: 9/10 Oct.

import ChessPieces
import Test.QuickCheck

-- Exercise 1:

pic1 :: Picture
pic1 = superimpose (above (beside knight (invert knight)) (beside (invert knight) knight)) (repeatV 2 (repeatH 2 whiteSquare))

pic2 :: Picture
pic2 = superimpose (above (beside knight (invert knight)) (beside (flipV (invert knight)) (flipV knight))) (repeatV 2 (repeatH 2 whiteSquare))


-- Exercise 2:

fourPieces :: Picture -> Picture
fourPieces pic = superimpose (above (beside pic (invert pic)) (beside (invert pic) pic)) (repeatV 2 (repeatH 2 whiteSquare))

-- Exercise 3: 
-- a)

fourSquares :: Picture
fourSquares = fourPieces whiteSquare

-- b)
fourKnights :: Picture
fourKnights = superimpose fourSquares (fourPieces knight)

-- c)
-- NOTE: you may have to put some arguments before the '='
piecesOnSquares :: Picture -> Picture
piecesOnSquares pic = superimpose fourSquares (fourPieces pic)

-- Exercise 4:
-- a)

emptyRow :: Picture
emptyRow = repeatH 4 (beside whiteSquare blackSquare)

-- b)

otherEmptyRow :: Picture
otherEmptyRow = invert emptyRow

-- c)

middleBoard :: Picture
middleBoard = repeatV 2 (above emptyRow otherEmptyRow)

-- d)

whiteRow :: Picture
whiteRow = aroundKQ king queen

blackRow :: Picture
blackRow = invert (aroundKQ queen king)

firstThree :: Picture
firstThree = beside (beside rook  knight) bishop

lastThree :: Picture
lastThree = beside (beside bishop  knight) rook

aroundKQ :: Picture -> Picture -> Picture
aroundKQ fig1 fig2 = superimpose (beside (beside (beside firstThree fig1) fig2) lastThree) (otherEmptyRow)

-- e)

populatedBoard :: Picture
populatedBoard = above (above (above (above blackRow (invert whitePawns)) middleBoard) whitePawns) whiteRow

whitePawns :: Picture
whitePawns = superimpose (repeatH 8 pawn) emptyRow

-- Exercise 5:

prop_assoc_above :: Picture -> Picture -> Picture -> Bool
prop_assoc_above p q r = p `above` (q `above` r) == (p `above` q) `above` r

prop_invol_invert :: Picture -> Bool
prop_invol_invert p = invert (invert p) == p

prop_assoc_beside :: Picture -> Picture -> Picture -> Bool
prop_assoc_beside p q r = p `beside` (q `beside` r) == (p `beside` q) `beside` r

{-
 b) the functions check if the function 'above' is associative and if invert invert p is p.
    prop_assoc_above is always true since the order stays the same. p is always above the rest, q is always above r. 
    So 'above' is associative.
    prop_invol_invert is true since not(not p) is p. - and - is +
 -}



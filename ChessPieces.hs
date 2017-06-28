--  Chess piece drawing implementation based on OpenGL, GLFW, and 
--  the SOE library by Paul Liu and Duncan Coutts, using fragments
--  of SOE's accompanying drawing modules.
--
--  Author: Willem Heijltjes
-- 
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
--  Lesser General Public License for more details.


module ChessPieces (Picture,
                    display,
                    height, width,
                    flipH, flipV, rotateL, rotateR,
                    invert,
                    beside, above, superimpose,
                    repeatH, repeatV,
                    whiteSquare, greySquare, blackSquare, clearSquare,
                    bishop, king, knight, pawn, queen, rook
                   ) where

import qualified Graphics.Rendering.OpenGL as GL
import Pictures
import Data.List as List
import Data.Set as Set
import Control.Monad( liftM, liftM2, liftM3 )
import Test.QuickCheck( Arbitrary (arbitrary, coarbitrary), elements )


-- Color regions

data CRegion = BLU -- bluescreen (background)
             | WSQ -- white square (background)
             | BSQ -- black square (background)
             | WPC -- white piece (foreground)
             | BPC -- black piece (foreground)
             | BOR -- piece borders (foreground)
               deriving (Eq, Show, Ord)

gray :: Float -> GL.Color3 Float
gray a = GL.Color3 a a a

regionToRGB :: CRegion -> GL.Color3 Float
regionToRGB BLU = GL.Color3 0.6 0.6 1
regionToRGB WSQ = gray 1
regionToRGB BSQ = gray 0.5
regionToRGB WPC = gray 1
regionToRGB BPC = gray 0
regionToRGB BOR = gray 0.2


-- Colored shapes

data CShape = Sq CRegion Point Point -- Square
            | Pt CRegion Point       -- Point
              deriving (Show)

instance Eq CShape where
    Sq ca p1a p2a == Sq cb p1b p2b = rectNormal p1a p2a == rectNormal p1b p2b
    Pt ca psa == Pt cb psb = ca == cb && psa == psb

instance Ord CShape where
    compare (Sq ca p1a p2a) (Sq cb p1b p2b) 
            | rectNormal p1a p2a < rectNormal p1b p2b = LT
            | rectNormal p1a p2a > rectNormal p1b p2b = GT
            | otherwise = compare ca cb
    compare (Pt ca pa) (Pt cb pb)
            | pa < pb = LT
            | pa > pb = GT
            | otherwise = compare ca cb
    compare (Pt _ _) (Sq _ _ _) = LT
    compare (Sq _ _ _) (Pt _ _) = GT

rectNormal (x1,y1) (x2,y2) = ((min x1 x2, min y1 y2),(max x1 x2, max y1 y2))


-- Generators for QuickCheck

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary     = liftM fromList arbitrary
  coarbitrary n = undefined

instance Arbitrary CShape where
  arbitrary     = do x <- elements [True,False]
                     if x then liftM3 Sq arbitrary arbitrary arbitrary
                          else liftM2 Pt arbitrary arbitrary
  coarbitrary n = undefined


instance Arbitrary CRegion where
  arbitrary     = elements [BLU,WSQ,BSQ,WPC,BPC,BOR]
  coarbitrary n = undefined


-- Pictures

type Picture = (Point, Set CShape)

height, width :: Picture -> Int
height (p,_) = snd p
width  (p,_) = fst p

-- Main drawing function

display :: Picture -> IO ()
display ((x,y), pcs) = do w <- openWindow "Chess" (x,y)
                          drawInWindow w $ toGraphic $ shapeToDrawing BSQ (Sq BLU (0,0) (x,y))
                          drawInWindow w $ toGraphic $ foldl (>>=) (return BLU) (List.map (flip shapeToDrawing) $ sortByColor $ Set.toList pcs)
                          spaceClose w

toGraphic :: IO CRegion -> Graphic
toGraphic cr = Graphic (cr >> return ())

shapeToDrawing :: CRegion -> CShape -> IO CRegion
shapeToDrawing c1 (Sq c2 p1 p2) = (if c1 /= c2 then GL.color (regionToRGB c2) else return ()) >> 
                                  GL.renderPrimitive GL.Polygon (mapM_ pointToVertex (f p1 p2)) >>
                                  return c2
                                      where 
                                        f (x1,y1) (x2,y2) = [(x1,y1),(x2,y1),(x2,y2),(x1,y2)]
shapeToDrawing c1 (Pt c2 pt)    =  (if c1 /= c2 then GL.color (regionToRGB c2) else return ()) >>
                                   GL.renderPrimitive GL.Points (pointToVertex pt) >>
                                   return c2
                                      
pointToVertex :: Point -> IO()
pointToVertex (x,y) = GL.vertex (vertex3 (fromIntegral x) (fromIntegral y) 0)

sortByColor :: [CShape] -> [CShape]
sortByColor pcs = List.filter hasBGColor pcs ++ List.filter hasFGColor pcs
    where
      hasBGColor (Sq c _ _) = c `elem` [BLU,WSQ,BSQ]
      hasBGColor (Pt c _)   = c `elem` [BLU,WSQ,BSQ]
      hasFGColor (Sq c _ _) = c `elem` [WPC,BPC,BOR]
      hasFGColor (Pt c _)   = c `elem` [WPC,BPC,BOR]

spaceClose :: Window -> IO ()
spaceClose w = do k <- getKey w
                  if (k==' ' || k == '\x0') then closeWindow w
                                            else spaceClose w

-- Manipulating the drawings

flipV :: Picture -> Picture
flipV ((x,y), shapes) = ((x,y), Set.map f shapes)
    where
      f (Sq c p1 p2)   = Sq c (g p1) (g p2)
      f (Pt c (x1,y1)) = Pt c (x-x1-1,y1)
      g (x1,y1) = (x-x1,y1)

flipH :: Picture -> Picture
flipH ((x,y), shapes) = ((x,y), Set.map f shapes)
    where
      f (Sq c p1 p2) = Sq c (g p1) (g p2)
      f (Pt c (x1,y1)) = Pt c (x1,y-y1-1)
      g (x1,y1) = (x1,y-y1)


flipDiag :: Picture -> Picture
flipDiag ((x,y), shapes) = ((y,x), Set.map f shapes)
    where
      f (Sq c p1 p2) = Sq c (g p1) (g p2)
      f (Pt c pt)    = Pt c (g pt)
      g (x1,y1) = (y1,x1)

rotateL :: Picture -> Picture
rotateL = flipDiag . flipV

rotateR :: Picture -> Picture
rotateR = flipDiag . flipH

invertPiece :: Picture -> Picture
invertPiece ((x,y), shapes) = ((x,y), Set.map f shapes)
    where
      f (Sq c p1 p2) = Sq (g c) p1 p2
      f (Pt c pt)    = Pt (g c) pt
      g WPC = BPC
      g BPC = WPC
      g c = c

invertSquare :: Picture -> Picture
invertSquare ((x,y), shapes) = ((x,y), Set.map f shapes)
    where
      f (Sq c p1 p2) = Sq (g c) p1 p2
      f (Pt c pt)    = Pt (g c) pt
      g WSQ = BSQ
      g BSQ = WSQ
      g c = c

invert :: Picture -> Picture
invert = invertPiece . invertSquare

beside :: Picture -> Picture -> Picture
beside ((x1,y1), shapes1) ((x2,y2), shapes2) = ((x1+x2, max y1 y2), Set.union shapes1 (Set.map f shapes2))
    where
      f (Sq c p1 p2) = Sq c (g p1) (g p2)
      f (Pt c pt)    = Pt c (g pt)
      g (x,y) = (x+x1,y)

above :: Picture -> Picture -> Picture
above ((x1,y1), shapes1) ((x2,y2), shapes2) = ((max x1 x2, y1+y2), Set.union shapes1 (Set.map f shapes2))
    where
      f (Sq c p1 p2) = Sq c (g p1) (g p2)
      f (Pt c pt)    = Pt c (g pt)
      g (x,y) = (x,y+y1)

superimpose :: Picture -> Picture -> Picture
superimpose ((x1,y1), shapes1) ((x2,y2), shapes2) = ((max x1 x2, max y1 y2), Set.union shapes2 shapes1)
   
repeatH :: Int -> Picture -> Picture
repeatH n pc | n <= 1    = pc
             | otherwise = beside pc (repeatH (n-1) pc)

repeatV :: Int -> Picture -> Picture
repeatV n pc | n <= 1    = pc
             | otherwise = above pc (repeatV (n-1) pc)

rasterToPiece :: [String] -> Picture
rasterToPiece strs = ((x,y), Set.fromList (f 0 strs))
    where
      x = length (head strs)
      y = length strs
      f _ [] = []
      f j (str:strs) = g 0 j str ++ f (j+1) strs
      g _ _ [] = []
      g i j (c:cs) | c == '#' = Pt BOR (i,j) : (g (i+1) j cs)
                   | c == '.' = Pt WPC (i,j) : (g (i+1) j cs)
                   | otherwise = (g (i+1) j cs)

-- The pieces themselves

whiteSquare, greySquare, blackSquare, clearSquare :: Picture
whiteSquare = ((58,58), Set.fromList [Sq WSQ (0,0) (58,58)])
greySquare  = ((58,58), Set.fromList [Sq BSQ (0,0) (58,58)])
blackSquare = greySquare
clearSquare = ((58,58), Set.empty)
bishop  = rasterToPiece bishopRaster
king    = rasterToPiece kingRaster
knight  = rasterToPiece knightRaster
pawn    = rasterToPiece pawnRaster
queen   = rasterToPiece queenRaster
rook    = rasterToPiece rookRaster

bishopRaster = (
 ["                                                          ",
  "                                                          ",
  "                                                          ",
  "                            ##                            ",
  "                          ######                          ",
  "                         ###..###                         ",
  "                         ##....##                         ",
  "                         ##....##                         ",
  "                         ###..###                         ",
  "                          ######                          ",
  "                           ####                           ",
  "                         ########                         ",
  "                        ###....###                        ",
  "                      ####......####                      ",
  "                    ####..........####                    ",
  "                   ###..............###                   ",
  "                  ###................###                  ",
  "                 ###..................###                 ",
  "                ###.........##.........###                ",
  "                ##..........##..........##                ",
  "               ###..........##..........###               ",
  "               ##...........##...........##               ",
  "               ##.......##########.......##               ",
  "               ##.......##########.......##               ",
  "               ##...........##...........##               ",
  "               ##...........##...........##               ",
  "               ##...........##...........##               ",
  "               ###..........##..........###               ",
  "                ##..........##..........##                ",
  "                ###....................###                ",
  "                 ##....................###                ",
  "                 ###..................###                 ",
  "                  ###................###                  ",
  "                   ####################                   ",
  "                   ####################                   ",
  "                   ##................##                   ",
  "                  ###................###                  ",
  "                  ##..................##                  ",
  "                  ######################                  ",
  "                 ########################                 ",
  "                 ###..................###                 ",
  "                 #####..............#####                 ",
  "                 ########################                 ",
  "                      ##############                      ",
  "                          ######                          ",
  "                        ####..####                        ",
  "        ##################......##################        ",
  "      ##################..........##################      ",
  "    ####..........................................####    ",
  "    ###.....................##.....................###    ",
  "     ##...................######...................##     ",
  "     ###.########.......####  ####.......########.###     ",
  "      ####################      ####################      ",
  "       ##        #######          #######        ##       ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          "]
               )

kingRaster = (
 ["                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                           ####                           ",
  "                           #..#                           ",
  "                           #..#                           ",
  "                         ###..###                         ",
  "                         #......#                         ",
  "                         ###..###                         ",
  "                           #..#                           ",
  "                           #..#                           ",
  "                           #..#                           ",
  "                           #..#                           ",
  "                           #..#                           ",
  "                          ######                          ",
  "                         ###..###                         ",
  "                         ##....##                         ",
  "                        ###....###                        ",
  "           #######      ##......##      #######           ",
  "         ###########    ##......##    ###########         ",
  "       ####.......####  ##......##  ####.......####       ",
  "      ###...........######......######...........###      ",
  "     ###..............####......####..............###     ",
  "     ##................####....####................##     ",
  "    ###.................###....###.................###    ",
  "    ##...................###..###...................##    ",
  "    ##...................###..###...................##    ",
  "    ##....................######....................##    ",
  "    ##....................######....................##    ",
  "    ##.....................####.....................##    ",
  "    ###....................####....................###    ",
  "     ##.....................##....................###     ",
  "     ###....................##....................##      ",
  "      ###...................##...................###      ",
  "       ###..................##..................###       ",
  "        ###...........##############...........###        ",
  "         ###.....########################.....###         ",
  "          ############..............############          ",
  "           ######........................######           ",
  "            ##..............................##            ",
  "            ##..............................##            ",
  "            ##........##############........##            ",
  "            ##...########################...##            ",
  "            ##########..............##########            ",
  "            #####........................#####            ",
  "            ##.........############.........##            ",
  "            ##....######################....##            ",
  "            ##.########............########.##            ",
  "            ######......................######            ",
  "            ######......................######            ",
  "               ########............########               ",
  "                  ######################                  ",
  "                       ############                       ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          "]
             )

knightRaster = (
 ["                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "               #         ##                               ",
  "              ###       ####                              ",
  "              #####     ####                              ",
  "               #####   ######                             ",
  "               ##.### ###..##                             ",
  "               ##..#####...######                         ",
  "               ##...####...#########                      ",
  "               ##..#####...##...######                    ",
  "              ###.###.......#.....######                  ",
  "             ###..#.................#####                 ",
  "            ###.......................####                ",
  "            ##.........................####               ",
  "           ###..####....................####              ",
  "           ##..####......................####             ",
  "           ##..####......................#####            ",
  "           ##..###........................####            ",
  "          ###..#..............##...........####           ",
  "          ##..................##...........####           ",
  "          ##..................##............####          ",
  "         ###..................##............####          ",
  "         ##...................##.............####         ",
  "        ###..................###.............####         ",
  "       ###..................###..............####         ",
  "      ###...................###...............####        ",
  "      ##...................####...............####        ",
  "     ###.................######...............####        ",
  "     ##................####  ##................####       ",
  "    ###.##...........####    ##................####       ",
  "    ##.###.........####     ###................####       ",
  "    ##.###........###       ##.................####       ",
  "    ##.##........###       ###.................#####      ",
  "    ##......##..###        ##...................####      ",
  "    ##.....###.###        ###...................####      ",
  "    ###...#######        ###....................####      ",
  "     ########.##        ###.....................####      ",
  "       #########       ###......................####      ",
  "            ###       ###.......................#####     ",
  "                     ###........................#####     ",
  "                    ###..........................####     ",
  "                    ##...........................####     ",
  "                   ###...........................####     ",
  "                  ###............................####     ",
  "                  ##.............................####     ",
  "                 ###.............................####     ",
  "                 ##..............................####     ",
  "                 ##..............................####     ",
  "                 ####################################     ",
  "                 ####################################     ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          "]
               )

pawnRaster = (
 ["                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                           ####                           ",
  "                         ########                         ",
  "                        ###....###                        ",
  "                       ###......###                       ",
  "                       ##........##                       ",
  "                       ##........##                       ",
  "                       ##........##                       ",
  "                       ##........##                       ",
  "                       ###......###                       ",
  "                        ###....###                        ",
  "                      #####....#####                      ",
  "                     ###..........###                     ",
  "                    ###............###                    ",
  "                    ##..............##                    ",
  "                   ###..............###                   ",
  "                   ##................##                   ",
  "                   ##................##                   ",
  "                   ##................##                   ",
  "                   ##................##                   ",
  "                   ##................##                   ",
  "                   ##................##                   ",
  "                   ###..............###                   ",
  "                    ##..............##                    ",
  "                    ###............###                    ",
  "                     ####........####                     ",
  "                     ####........####                     ",
  "                   ####............####                   ",
  "                  ###................###                  ",
  "                 ###..................###                 ",
  "                ###....................###                ",
  "               ###......................###               ",
  "               ##........................##               ",
  "              ###........................###              ",
  "              ##..........................##              ",
  "             ###..........................###             ",
  "             ##............................##             ",
  "             ##............................##             ",
  "            ###............................###            ",
  "            ##..............................##            ",
  "            ##..............................##            ",
  "            ##..............................##            ",
  "            ##..............................##            ",
  "            ##################################            ",
  "            ##################################            ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          "]
             )

queenRaster = (
 ["                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                            ##                            ",
  "                          ######                          ",
  "              #####      ###..###      #####              ",
  "             #######     ##....##     #######             ",
  "             ##...##     ##....##     ##...##             ",
  "             ##...##     ########     ##...##             ",
  "    ##       ##...##      ######      ##...##       ##    ",
  "  ######     #######        ##        #######     ######  ",
  " ###..###     #####        ####        #####     ###..### ",
  " ##....##       ###        ####        ###       ##....## ",
  " ###...##       ###        ####        ###       ##...### ",
  "  #######       ###        ####        ###       #######  ",
  "   #####        ####      ######      ####        #####   ",
  "     ###        ####      ##..##      ####        ###     ",
  "     ####       ####      ##..##      ####       ####     ",
  "     ####       #####     ##..##     #####       ####     ",
  "     #####      ##.##     ##..##     ##.##      #####     ",
  "      #####     ##.##    ###..###    ##.##     #####      ",
  "      ##.##    ###.###   ##....##   ###.###    ##.##      ",
  "      ##.###   ##...##   ##....##   ##...##   ###.##      ",
  "      ##..##   ##...###  ##....##  ###...##   ##..##      ",
  "      ##..###  ##....## ###....### ##....##  ###..##      ",
  "      ###..##  ##....## ##......## ##....##  ##..###      ",
  "       ##..### ##....#####......#####....## ###..##       ",
  "       ##...#####.....####......####.....#####...##       ",
  "       ##....####.....####......####.....####....##       ",
  "       ##....####..#..###..####..###..#..####....##       ",
  "       ###....###.######################.###....###       ",
  "        ##.################....################.##        ",
  "        ##########......................##########        ",
  "        ###....................................###        ",
  "         ##....................................##         ",
  "         ###..................................###         ",
  "          ###.......##################.......###          ",
  "           ####################################           ",
  "            ########..................########            ",
  "            ###............................###            ",
  "             ##............................##             ",
  "             ##......################......##             ",
  "             ################################             ",
  "             ########................########             ",
  "             ##............................##             ",
  "            ###.....##################.....###            ",
  "            ##################################            ",
  "           #########..................#########           ",
  "           ##................................##           ",
  "           ##................................##           ",
  "           #########..................#########           ",
  "             ################################             ",
  "                    ##################                    ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          "]
              )

rookRaster = (
 ["                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "           #########     ########     #########           ",
  "           #########     ########     #########           ",
  "           ##.....##     ##....##     ##.....##           ",
  "           ##.....##     ##....##     ##.....##           ",
  "           ##.....#########....#########.....##           ",
  "           ##.....#########....#########.....##           ",
  "           ##................................##           ",
  "           ####################################           ",
  "           ####################################           ",
  "            ###............................###            ",
  "             ####........................####             ",
  "               ############################               ",
  "                ##########################                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##......................##                ",
  "                ##########################                ",
  "               ############################               ",
  "              ###........................###              ",
  "            ##################################            ",
  "            ##################################            ",
  "            ##..............................##            ",
  "            ##..............................##            ",
  "            ##..............................##            ",
  "            ##..............................##            ",
  "        ##########################################        ",
  "        ##########################################        ",
  "        ##......................................##        ",
  "        ##########################################        ",
  "        ##########################################        ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          ",
  "                                                          "]
             )

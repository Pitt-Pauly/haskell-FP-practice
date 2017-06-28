--  Turtle graphics drawing implementation based on OpenGL, GLFW, and 
--  the SOE library by Paul Liu and Duncan Coutts, using fragments
--  of SOE's accompanying drawing modules.
--
--  Authors: Willem Heijltjes & Phil Wadler
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



module LSystem (
    display, randomPic,
    Command (..),
    Pen (..), black, white, red, green, blue,
    Distance, Angle,
    triangle, tree
   )
    where

import qualified Graphics.Rendering.OpenGL as GL
import Pictures
import Control.Monad( liftM, liftM2, liftM3 )
import System.Random
import Test.QuickCheck --( Arbitrary (arbitrary, coarbitrary), elements, oneof, sized, generate )

infixr 5 :#:



-- Points

data Pnt = Pnt Float Float
  deriving (Eq,Ord,Show)

instance Num Pnt where
  Pnt x y + Pnt x' y'  =  Pnt (x+x') (y+y')
  Pnt x y - Pnt x' y'  =  Pnt (x-x') (y-y')
  Pnt x y * Pnt x' y'  =  Pnt (x*x') (y*y')
  fromInteger n        =  Pnt (fromInteger n) (fromInteger n)
  abs (Pnt x y)        =  Pnt (abs x) (abs y)
  signum (Pnt x y)     =  Pnt (signum x) (signum y)

instance Fractional Pnt where
  Pnt x y / Pnt x' y'  =  Pnt (x/x') (y/y')
  fromRational r  =  Pnt (fromRational r) (fromRational r)

scalar :: Float -> Pnt
scalar x  =  Pnt x x

scalarMin :: Pnt -> Pnt
scalarMin (Pnt x y)  =  scalar (x `min` y)

dimensions :: Pnt -> (Int,Int)
dimensions (Pnt x y)  =  (ceiling x, ceiling y)

lub :: Pnt -> Pnt -> Pnt
Pnt x y `lub` Pnt x' y'  =  Pnt (x `max` x') (y `max` y')

glb :: Pnt -> Pnt -> Pnt
Pnt x y `glb` Pnt x' y'  =  Pnt (x `min` x') (y `min` y')

-- The last two functions are not called min and max
-- because the invariant for min and max states
-- (min x y, max x y) = (x,y) or (y,x).



-- Colors

data Pen = Colour Float Float Float
         | Inkless
           deriving (Eq, Ord, Show)

penToRGB :: Pen -> GL.Color3 Float
penToRGB (Colour r g b)  =  GL.Color3 r g b
penToRGB Inkless  =  error "penToRGB: inkless"

white, black, red, green, blue :: Pen
white = Colour 1.0 1.0 1.0
black = Colour 0.0 0.0 0.0
red   = Colour 1.0 0.0 0.0
green = Colour 0.0 1.0 0.0
blue  = Colour 0.0 0.0 1.0



-- Window parameters

margin  =  Pnt 20 20
canvas  =  Pnt 800 600

backgroundcolor :: GL.Color3 Float
backgroundcolor = penToRGB white



-- Main drawing and window functions

-- Note to Willem:
-- setting up the background and closing on spaces should be part of openWindow
-- Indeed, setting up openWindow to just accept a bounding box and a
-- list of lines might be sensible

display :: Command -> IO ()
display c  = window (rescale (execute c))

window :: [Line] -> IO ()
window l  = do let frame  =  canvas + 2 * margin
               w <- openWindow "LSystem" (dimensions frame)
               drawInWindow w (background frame)
               drawInWindow w (graphic l)
               spaceClose w

graphic :: [Line] -> Graphic
graphic lines  =  Graphic (sequence_ (map f lines))
  where
  f (Line pen startP endP)  =
    GL.color (penToRGB pen) >>
    GL.renderPrimitive GL.LineStrip (toVertex startP >> toVertex endP)

spaceClose :: Window -> IO ()
spaceClose w = do k <- getKey w
                  if (k==' ' || k == '\x0') then closeWindow w
                                            else spaceClose w

background :: Pnt -> Graphic
background (Pnt x y)  =  Graphic $ 
  GL.color backgroundcolor >>
  GL.renderPrimitive GL.Polygon
    (sequence_ (map toVertex [Pnt 0 0, Pnt x 0, Pnt x y, Pnt 0 y]))

toVertex (Pnt x y)  =  GL.vertex (vertex3 x y 0)



-- Commands for moving the turtle around
--  Turtles turn counter-clockwise and start facing up

type Distance = Float
type Turtle = (Pen,Angle,Pnt)

data Line = Line Pen Pnt Pnt
  deriving (Eq,Ord,Show)

data Command = Go Distance
             | Turn Angle 
             | Sit
             | Command :#: Command
             | Branch Command
             | GrabPen Pen
               deriving (Eq, Ord, Show)


-- Converting commands to GL graphics

execute :: Command -> [Line]
execute c  =  lines
  where
  (lines, turtle)  =  f c (black, 270, Pnt 0 0)

  f :: Command -> Turtle -> ([Line], Turtle)
  f (c :#: d) turtle             =  (clines ++ dlines, dturtle)
                                    where
                                    (clines, cturtle) = f c turtle
                                    (dlines, dturtle) = f d cturtle
  f (Branch c) turtle            =  (clines, turtle)
                                    where
                                    (clines, cturtle) = f c turtle
  f (Go dst) (pen,ang,pnt)       =  (if pen == Inkless 
                                    then [] 
                                    else [Line pen pnt endpnt],
                                    (pen,ang,endpnt))
                                    where
                                    endpnt = pnt + scalar dst * polar ang
  f (Turn delta) (pen,ang,pnt)   =  ([], (pen,ang-delta,pnt))
  f (GrabPen new) (old,ang,pnt)  =  ([], (new,ang,pnt))
  f (Sit) turtle		 =  ([], turtle)

rescale :: [Line] -> [Line]
rescale lines  =  map f lines
  where
  f (Line pen p q)  =  Line pen (g p) (g q)
  g p               =  p0 + s * p
  points            =  [ r | Line pen p q <- lines, r <- [p, q] ]
  hi                =  foldr lub 0 points
  lo                =  foldr glb 0 points
  s                 =  scalarMin (canvas / (hi - lo))
  p0                =  margin + canvas / 2 - s * (lo + hi) / 2

polar :: Angle -> Pnt
polar ang  =  Pnt (cos radians) (sin radians)
  where
  radians  =  ang * 2 * pi / 360


-- Sample LSystems

triangle :: Int -> Command
triangle x  =  p :#: f x
  where
  f 0      = Go 10
  f (x+1)  = f x :#: p :#: f x :#: n :#: f x :#: n :#: f x :#: p :#: f x
  n        = Turn 90
  p        = Turn (-90)

tree :: Int -> Command
tree x  =  f x
  where
  f 0      = GrabPen red :#: Go 10
  f (x+1)  = g x :#: Branch (n :#: f x)
                 :#: Branch (p :#: f x)
                 :#: Branch (g x :#: f x)
  g 0      = GrabPen blue :#: Go 10
  g (x+1)  = g x :#: g x
  n        = Turn 45
  p        = Turn (-45)



-- Generators for QuickCheck

instance Arbitrary Pen where
    arbitrary      =  liftM3 Colour val val val
        where val  =  elements [0.0, 0.125 .. 1.0] 
    coarbitrary n  =  undefined

instance Arbitrary Command where
    arbitrary      =  sized com 
        where
          com n  |  n <= 0     =  oneof ends
                 |  otherwise  =  oneof (splits n) 
          ends      =  [liftM (Go . abs) arbitrary, liftM Turn arbitrary]
                       --liftM GrabPen arbitrary
          splits n  =  --liftM Branch (com (n-1)) : 
                       replicate 4 (liftM2 (:#:) (com (n `div` 2)) (com (n `div`2)))
    coarbitrary n  =  undefined

sample :: Arbitrary a => Int -> IO a
sample n  =  do g <- newStdGen
                return (generate n g arbitrary)

randomPic :: Int -> IO()
randomPic n  =  do c <- sample n
                   display c




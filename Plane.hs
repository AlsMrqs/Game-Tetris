module Plane where

import Prelude  hiding  (Right, Left)
import Data.List        (intercalate, intersperse)

data Plane = Plane
    { limit     :: (Int,Int)
    , grid      :: [[Elem]] 
    , ground    :: [(Int,Int)] }

instance Show Plane where
    show p = intercalate "\n" . map (intersperse ' ' . map pixel) $ grid p

data Elem = Elem
    { point     :: (Int,Int)
    , pixel     :: Char } deriving Show

data Direction = Up | Left | Right | Down deriving Show

    -- Creates a new Plane --
plane :: (Int,Int) -> Plane
plane (x,y) = 
    Plane (x,y) [[Elem (a,b) '.' | a <- xs] | b <- ys] $ limits (x,y)
    where
        xs = [-x..x]
        ys = [y,(y-1)..(-y)]

limits :: (Int,Int) -> [(Int,Int)]
limits (x,y) = (zip xs $ repeat (-y-1)) ++ 
               (zip (repeat $  x+1) ys) ++ 
               (zip (repeat $ -x-1) ys)
    where
        xs = [-x..x]
        ys = [y,(y-1)..(-y)]

    -- Adds an [Elem] inside a Plane --
with :: Plane -> [Elem] -> Plane
with (Plane (x,y) _grid _ground) _block = Plane (x,y) newGrid _ground
    where
        newGrid = map (map $ \_elem -> foldl changePixel _elem _block) _grid

    -- Keeps the snd Elem pixel if both have same point --
changePixel :: Elem -> Elem -> Elem
changePixel el1 el2 = if (point el1) == (point el2) then el2 else el1

    -- Plane (Int,Int) [[Element]] [(Int,Int)] --
addGround :: Plane -> [(Int,Int)] -> Plane
addGround (Plane (x,y) l _g) _g1 = Plane (x,y) l (_g ++ _g1)

    -- Verify if any [Elem] is out of (Plane) range --
falls :: Plane -> [Elem] -> Bool
falls (Plane (x,y) _ _) = 
    any ((\(a,b) ->  ((abs a) > x) || ((abs b) > y) ) . point)


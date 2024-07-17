module Control where

import Prelude  hiding  (Left, Right)
import System.Random    (newStdGen, randomR)
import Plane            (Plane(..), Elem(..), Direction(..)) --, move)
import Blocks

type Block = [Elem]

command :: Char -> Block -> Block
command o b = case o of
    '2' -> move b Down
    '4' -> move b Left
    '6' -> move b Right
    '0' -> rotate b
    _   -> b

rotate :: Block -> Block
rotate _block = map (\(Elem (x,y) _pixel) -> Elem (f (x,y)) _pixel) _block
    where
        (a,b)   = point . head $ tail _block
        f (x,y) = ((-b)+y+a,a+(-x)+b)

blocks = [b0,b1,b2,b3,b4,b5,b6]

block :: IO Block
block = return . (!!) blocks . fst . randomR (0,6) =<< newStdGen

type Sketch = [[Int]]

sketch :: Plane -> Sketch
sketch p = [[0 | _ <- xs] | _ <- ys]
    where
        xs    = [-x..x]
        ys    = [y,(y-1)..(-y)]
        (x,y) = limit p

coords :: Block -> [(Int,Int)]
coords = map point

type Point = (Int,Int)
    
    -- Moves a Point into a Direction --
movePoint :: Direction -> Point -> Point
movePoint dir (x,y) = case dir of
    Up    -> (x,y+1)
    Down  -> (x,y-1)
    Left  -> (x-1,y)
    Right -> (x+1,y)

    -- Moves a Elem into a Direction --
direction :: Elem -> Direction -> Elem
direction (Elem _point _pixel) _direct = Elem (movePoint _direct _point) _pixel

move :: [Elem] -> Direction -> [Elem]
move _block _direct = map (`direction` _direct) _block


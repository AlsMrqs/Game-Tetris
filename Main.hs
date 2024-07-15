module Main where

import Prelude  hiding      (Left,Right,lines)
import Data.Bool            (bool)
import System.IO            (hSetBuffering,stdin,BufferMode(NoBuffering),stdout,hSetEcho)
import Control.Concurrent   (killThread,threadDelay,forkIO,ThreadId,MVar)
import Control.Concurrent.MVar   (MVar,newMVar,readMVar,swapMVar)
import Data.List            (nub)  
import Data.List hiding     (lines)

import Plane
import Blocks
import Control

main :: IO ()
main = do
    hSetEcho      stdin False
    hSetBuffering stdin NoBuffering

    p <- newMVar $ plane (4,8)
    b <- newMVar =<< block

    forkIO (interface p b) >>= (\tid -> start tid p b)

    -- This function manage inputs --
start :: ThreadId -> MVar Plane -> MVar Block -> IO ()
start tid mvar_p mvar_b = do

    input <- getChar
    if input == 'q' then putStr "\n" >> killThread tid
    else do
        p <- readMVar mvar_p
        b <- readMVar mvar_b

        let b' = input `command` b 
            b2 = bool b' b (collision p b')

        swapMVar mvar_b b2

        putStr . (++) "\ESC[2J\ESC[H\ESC[?25l" . show . with p =<< readMVar mvar_b

        start tid mvar_p mvar_b

    -- Interface Graphics (Animation) --
interface :: MVar Plane -> MVar Block -> IO ()
interface mvar_p mvar_b = do
    p <- readMVar mvar_p
    b <- readMVar mvar_b
    putStr . (++) "\ESC[2J\ESC[H\ESC[?25l" . show $ p `with` b

    if allocated p b then do
        _ <- swapMVar mvar_p $ addGround (p `with` b) (coordinates b)
        _ <- swapMVar mvar_b =<< block
            -- Printing the background data --
        p' <- readMVar mvar_p
        p'' <- eliminate (full $ lines b p') p'
        swapMVar mvar_p p''
        putStr . (++) "\ESC[2J\ESC[H\ESC[?25l" . show $ p'' 
        return ()
    else do
        _ <- swapMVar mvar_b . (`move`Down) =<< readMVar mvar_b
        return ()

    if gameOver p then putStrLn "\nGame Over!\nPress ( q ) to exit!"
    else do
        threadDelay 250000
        interface mvar_p mvar_b

    {- ANIMATION - Line Eliminate -}

eliminate :: [Int] -> Plane -> IO Plane
eliminate i (Plane l e _ground) = do
    let newground = filter (\(x,y) -> (/= y) `all` i) _ground
    gradual i (Plane l e newground) 

gradual :: [Int] -> Plane -> IO Plane
gradual []     p = return p
gradual (y:[]) p = step y p 0
gradual (y:ys) p = gradual ys =<< step y p 0

step :: Int -> Plane -> Int -> IO Plane
step y p x = do
    if x == (fst $ limit p) 
        then do
            putStrLn . (++) "\ESC[2J\ESC[H\ESC\ESC[?25l" . show $ p
            threadDelay 250000
            return $ Plane (limit p) [[clean e (x,y) | e <- ls] | ls <- (grid p)] (ground p)
        else do
            putStrLn . (++) "\ESC[2J\ESC[H\ESC\ESC[?25l" . show $ p
            threadDelay 250000
            step y (Plane (limit p) [[clean e (x,y) | e <- ls] | ls <- (grid p)] $ ground p) (x+1)
            
clean :: Elem -> (Int,Int) -> Elem
clean e (x,y) 
    | point e == (x,y)  = (Elem (x,y) '.')  
    | point e == (-x,y) = (Elem (-x,y) '.') 
    | otherwise         = e

        {- Game Verifications -}

gameOver :: Plane -> Bool
gameOver p = mutual (ground p) lines
    where
        (x,y) = limit p
        lines  = zip [-x..x] (repeat y)

    -- checks if the block can move down --
allocated :: Plane -> Block -> Bool
allocated p b = collision p (move b Down)

    -- checks if the block is overlapping a solid --
collision :: Plane -> Block -> Bool
collision p b = mutual solid (coord b)
    where
        solid = ground p
        coord = coordinates
        
mutual :: [(Int,Int)] -> [(Int,Int)] -> Bool
mutual l = foldl (\acc x -> acc || (elem x l)) False

full :: [[(Int,Int)]] -> [Int]
full = map (snd . head) . filter ((==) 11 . length)

lines :: Block -> Plane -> [[(Int,Int)]]
lines b p = map (\y -> filter ((==) y . snd) $ ground p) (allLines b)

allLines :: Block -> [Int]
allLines = nub . map (snd . point)


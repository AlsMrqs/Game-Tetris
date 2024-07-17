module Main where

import Prelude  hiding      (Left,Right,lines)
import Data.Bool            (bool)
import System.IO            (hSetBuffering,stdin,BufferMode(NoBuffering),stdout,hSetEcho)
import Control.Concurrent   (killThread,threadDelay,forkIO,ThreadId)
import Control.Concurrent.MVar   (MVar,newMVar,readMVar,swapMVar,takeMVar,putMVar)
import Data.List            (nub)  
import Data.List hiding     (lines)

import Plane
import Blocks
import Control
-- import Interface 

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
    b     <- takeMVar mvar_b
    if input == '5' then do
        putStrLn "Pause!"
        _ <- getChar
        putMVar mvar_b b
        start tid mvar_p mvar_b
    else do
        if input == 'q' then putStr "\n" >> killThread tid -- add (Pause) here --
        else do
            p <- readMVar mvar_p
            let b' = input `command` b
                b2 = bool b' b (collision p b')
            putMVar mvar_b b2 
            putStr . (++) "\ESC[2J\ESC[H\ESC[?25l" . show . with p =<< readMVar mvar_b

            start tid mvar_p mvar_b

        {- Game Interface -}

    -- Interface Graphics (Animation) --
interface :: MVar Plane -> MVar Block -> IO ()
interface mvar_p mvar_b = do
    p <- takeMVar mvar_p
    b <- takeMVar mvar_b
    putStr . (++) "\ESC[2J\ESC[H\ESC[?25l" . show $ p `with` b

    if allocated p b then do
        let p' = addGround (p `with` b) (coords b)
        p'' <- eliminate (reverse . sort . full $ lines b p') p'

        putMVar mvar_p p'' -- alteration --
        putMVar mvar_b =<< block
        putStr . (++) "\ESC[2J\ESC[H\ESC[?25l" . show $ p'' 

        return ()
    else do
        putMVar mvar_p p
        putMVar mvar_b (move b Down)
        return ()

    if gameOver p then putStrLn "\nGame Over!\nPress ( q ) to exit!"
    else do
        threadDelay 550000
        interface mvar_p mvar_b


    {- ANIMATION - Line Eliminate -}


eliminate :: [Int] -> Plane -> IO Plane
eliminate i (Plane l e _ground) = falling i =<< gradual i (Plane l e newground)
    where
        newground = filter (\(x,y) -> ruley y || rulex x) _ground

        ruley y = (`all` i) (/= y)
        rulex x = (x < ((-) 0 $ fst l) || (fst l) < x)

        gradual :: [Int] -> Plane -> IO Plane 
        gradual []     p = return p
        gradual (y:[]) p = step y p 0
        gradual (y:ys) p = gradual ys =<< step y p 0

        step :: Int -> Plane -> Int -> IO Plane
        step y p x = do
            if x == (fst $ limit p) 
                then do
                    putStrLn . (++) "\ESC[2J\ESC[H\ESC[?25l" . show $ p
                    threadDelay 50000
                    return $ Plane (limit p) [[clean e (x,y) | e <- ls] | ls <- (grid p)] (ground p)
                else do
                    putStrLn . (++) "\ESC[2J\ESC[H\ESC[?25l" . show $ p
                    threadDelay 50000
                    step y (Plane (limit p) [[clean e (x,y) | e <- ls] | ls <- (grid p)] $ ground p) (x+1)
                    
        clean :: Elem -> (Int,Int) -> Elem
        clean e (x,y) 
            | point e == (x,y)  = (Elem (x,y) '.')  
            | point e == (-x,y) = (Elem (-x,y) '.') 
            | otherwise         = e


    {- Shift Down -}


falling :: [Int] -> Plane -> IO Plane
falling []       p = return p
falling i@(x:xs) p = do
    putStrLn . (++) "\ESC[2J\ESC[H\ESC[?25l" . show $ p

    threadDelay 250000

    let p1 = step p $ tofall i p

    falling xs p1

tofall :: [Int] -> Plane -> [(Int,Int)]
tofall i (Plane l gri gro) = filter (\(x,y) -> ruley y && rulex x) gro
    where
        ruley n = all (<n) $ (reverse . sort) i
        rulex n = (-(fst l) <= n && n <= (fst l))

step :: Plane -> [(Int,Int)] -> Plane
step (Plane l gri gro) coord = Plane l newgrid newground
    where
        newground = changeGro coord gro
        newgrid   = map (changeGri newground) gri

        changeGri :: [(Int,Int)] -> [Elem] -> [Elem] 
        changeGri coo b = map modify b
            where
                modify :: Elem -> Elem
                modify e = if not ((point e) `elem` coo)
                    then changePixel e (Elem (point e) '.')
                    else changePixel e (Elem (point e) '#')

        changeGro :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
        changeGro a b = map (\pnt -> bool pnt (movePoint Down pnt) (pnt `elem` a)) b


    {- Game Verification -}


gameOver :: Plane -> Bool
gameOver p = mutual (ground p) lines
    where
        (x,y) = limit p
        lines  = zip [-x..x] (repeat y)

    -- checks if the block can move down --
allocated :: Plane -> Block -> Bool                 -- Ok --
allocated p b = collision p (move b Down)

    -- checks if the block is overlapping a solid --

collision :: Plane -> Block -> Bool                 -- Ok --
collision p b = mutual (ground p) (coords b)

mutual :: [(Int,Int)] -> [(Int,Int)] -> Bool        -- Ok --
mutual l = foldl (\acc x -> acc || (elem x l)) False

full :: [[(Int,Int)]] -> [Int]
full = map (snd . head) . filter ((==) 11 . length)

lines :: Block -> Plane -> [[(Int,Int)]]
lines b p = map (\y -> filter ((==) y . snd) $ ground p) (allLines b)

allLines :: Block -> [Int]
allLines = nub . map (snd . point)


module Lib
    (
	Loops,
        newLoops,       
	draw,
	step,
	mkTransFunc
    ) where

import Data.Array
import Data.Char
import System.Console.ANSI


newtype Loops = Loops { runLoops :: Array (Int, Int) Int }
    deriving (Show)


startLoop = ["022222222000000",
             "217014014200000",
             "202222220200000",
             "272000021200000",
             "212000021200000",
             "202000021200000",
             "272000021200000",
             "212222221222220",
             "207107107111112",
             "022222222222220"]   


lulzToArray :: [[a]] -> Array (Int, Int) a
lulzToArray ls = listArray b (concat ls)
    where
        l1 = (length ls)
        l2 = (length (head ls))
        b = ((0,0),(l1-1,l2-1))


fromLulz :: [[Char]] -> Loops
fromLulz = Loops . lulzToArray . ((fmap.fmap) digitToInt)


neighbors :: Array (Int, Int) Int -> (Int, Int) -> [Int]
neighbors arr (x, y) = [get x y] ++
                       [get (x-1) y] ++
                       [get x (y+1)] ++
                       [get (x+1) y] ++
                       [get x (y-1)]
    where
        ((_, _), (b1, b2)) = bounds arr
        rowb = b1 - 1
        colb = b2 - 1
        get row col = arr ! (row `mod` rowb, col `mod` colb)


shift :: (Int, Int) -> [((Int, Int), Int)] -> [((Int, Int), Int)]
shift (x, y) = fmap (\((i, j), k) -> ((i+x, j+y), k))


zeros :: (Int, Int) -> Array (Int, Int) Int
zeros (x, y) = listArray b (take (x * y) (repeat 0))
    where b = ((0,0),(x-1,y-1))


resize :: Loops -> (Int, Int) -> Loops
resize l dim@(x,y) = Loops $ (zeros dim) // (shift s (assocs (runLoops l)))
    where s = (div x 2, div y 2)


digsToInt :: [Int] -> Int
digsToInt = (go 0) . reverse
    where
        go pow []   = 0
        go pow digs = ((head digs) * (10 ^ pow)) + go (pow + 1) (tail digs)  


newLoops :: (Int, Int) -> Loops
newLoops = resize (fromLulz startLoop)


splitTrans :: String -> (String, String)
splitTrans s = ((take 5 s), (drop 5 s))


rotations :: [a] -> [[a]]
rotations list = go l [] list
    where 
        l           = length list
	rotOnce ls  = (tail ls) ++ [head ls]
        go 0 acc ls = acc
        go x acc ls = go (x - 1) (acc ++ [rotOnce ls]) (rotOnce ls)


transRotations :: (String, String) -> [(String, String)]
transRotations (s1, s2) = fmap (\r -> (([head s1] ++ r), s2)) $ (rotations . tail) s1


relToFunc :: Eq a => [(a, a)] -> (a -> a)
relToFunc [] = id
relToFunc (x:xs) = \y -> 
    if y == (fst x) 
        then (snd x)
            else (relToFunc xs) y


mkTransFunc :: [String] -> (Int -> Int)
mkTransFunc strs = relToFunc (asNums tups)
    where
        toNum = (digsToInt . (fmap digitToInt))
        asNums = fmap (\(s1, s2) -> (toNum s1, toNum s2))
        tups = concatMap (transRotations . splitTrans) strs


neighborList :: Array (Int, Int) Int -> [Int]
neighborList a = (fmap (digsToInt . (neighbors a))) (indices a)


step :: (Int -> Int) -> Loops -> Loops
step trans loop = Loops $ listArray (bounds arr) newList
    where 
        arr     = runLoops loop
        newList = fmap trans (neighborList arr)


drawCell :: ((Int, Int), Int) -> IO ()
drawCell (_, 0) = return ()
drawCell ((x, y), z) = do
    setSGR [SetColor Foreground Vivid (getColor z)]
    setCursorPosition y (x*2)
    putStr "█"
    setCursorPosition y (x*2 + 1)
    putStr "█"


getColor :: Int -> Color
getColor 0 = Black
getColor 1 = White
getColor 2 = Red
getColor 3 = Green
getColor 4 = Cyan
getColor 5 = Magenta
getColor 6 = Yellow
getColor 7 = Blue
getColor x = Red

draw :: Loops -> IO ()
draw = sequence_ . (fmap drawCell) . assocs . runLoops

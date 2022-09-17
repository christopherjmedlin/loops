module Lib
    (
        newLoops,       
    ) where

import Data.Array
import Data.Char


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
                       [get x (y+1)] ++
                       [get (x+1) y] ++
                       [get x (y-1)] ++
                       [get (x-1) y]
    where
        ((_, _), (b1, b2)) = bounds arr
        rowb = b1 - 1
        colb = b2 - 1
        get row col = arr ! (row `rem` rowb, col `rem` colb)


shift :: (Int, Int) -> [((Int, Int), Int)] -> [((Int, Int), Int)]
shift (x, y) = fmap (\((i, j), k) -> ((i+x, j+y), k))


zeros :: (Int, Int) -> Array (Int, Int) Int
zeros (x, y) = listArray b (take (x * y) (repeat 0))
    where b = ((0,0),(x-1,y-1))


resize :: Loops -> (Int, Int) -> Loops
resize l dim@(x,y) = Loops $ (zeros dim) // (shift s (assocs (runLoops l)))
    where s = (div x 2, div y 2)


digsToInt :: [Int] -> Int
digsToInt = go 0
    where
        go pow []   = 0
        go pow digs = ((head digs) * (10 ^ pow)) + go (pow + 1) (tail digs)  


newLoops :: (Int, Int) -> Loops
newLoops = resize (fromLulz startLoop)


splitTrans :: String -> (Int, Int)
splitTrans s = (read (take 5 s), read (drop 5 s))


relToFunc :: Eq a => [(a, a)] -> (a -> a)
relToFunc [] = id
relToFunc (x:xs) = \y -> 
    if y == (fst x) 
        then (snd x)
            else (relToFunc xs) y


mkTransFunc :: [String] -> (Int -> Int)
mkTransFunc strs = relToFunc (fmap splitTrans strs)


neighborList :: Array (Int, Int) Int -> [Int]
neighborList a = (fmap (digsToInt . (neighbors a))) (indices a)


step :: (Int -> Int) -> Loops -> Loops
step trans loop = Loops $ listArray (bounds arr) newList
    where 
        arr     = runLoops loop
        newList = fmap trans (neighborList arr)

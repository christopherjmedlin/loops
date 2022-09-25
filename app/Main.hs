module Main (main) where

import Lib
import System.Console.ANSI
import System.IO
import Control.Concurrent


run :: (Int -> Int) -> Loops -> IO ()
run trans loops = do
    clearScreen
    draw (step trans loops)
    hFlush stdout
    threadDelay 100000
    run trans (step trans loops)


main :: IO ()
main = do
    f <- readFile "transitions.txt"
    let t = mkTransFunc (lines f)
    term <- getTerminalSize
    case term of
        Just (x, y) -> run t (newLoops (x, y))
        Nothing -> run t (newLoops (50, 50))

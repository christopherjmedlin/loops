module Main (main) where

import Lib
import System.Console.ANSI
import System.IO
import Control.Concurrent
import System.Posix.Signals


run :: (Int -> Int) -> Loops -> IO ()
run trans loops = do
    let new = step trans loops
    draw new
    hFlush stdout
    run trans new


terminate :: IO ()
terminate = do
    clearScreen
    showCursor


main :: IO ()
main = do
    id <- myThreadId
    installHandler keyboardSignal (Catch $ terminate >> (killThread id)) Nothing
    clearScreen
    hideCursor
    f <- readFile "transitions.txt"
    let t = mkTransFunc (lines f)
    term <- getTerminalSize
    case term of
        Just (row, col) -> run t (newLoops ((col `div` 2), row))
        Nothing -> run t (newLoops (50, 50))

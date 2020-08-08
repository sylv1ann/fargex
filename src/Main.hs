{-|
Module: Main
Description: The entrypoint of the program.
-}

module Main where

import RegtFsm as R
import Fsmtrex as F
import System.IO

-- | Starts the appropriate function which handles desired operation based on the choice of the user given via the stdIn.
main :: IO () 
main = do
    putStr "Choose a number from the following options:\n    1 ) RegEx -> FSM transformation.\n    2 ) FSM   -> RegEx transformation.\nChoose the option: "
    hFlush stdout
    option <- getLine
    process option

-- | Starts the appropriate function.
process :: String -> IO ()
process opt
    | opt == "1" || opt == "2"   = 
        if opt == "1" then
             R.toFinAuto 
        else 
             F.toRegex
    | otherwise     = error "Non existing action"  

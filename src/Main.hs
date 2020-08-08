{-|
Module: Main
Description: The entrypoint of the program.
-}

module Main where

import RegtFsm as R
import Fsmtrex as F

-- | Starts the appropriate function which handles desired operation based on the choice of the user given via the stdIn.
main :: IO () 
main = do
    putStrLn "Choose a number from the following options."
    putStrLn "1 ) RegEx -> FSM transformation."
    putStrLn "2 ) FSM   -> RegEx transformation."
    putStr   "Choose from the above actions: "
    option <- getLine
    process option

process :: String -> IO ()
process opt
    | opt == "1" || opt == "2"   = 
        if opt == "1" then
             R.toFinAuto 
        else 
             F.toRegex
    | otherwise     = error "Non existing action"  

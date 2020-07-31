{-|
Module: Main
Description: Entrypoint to the program.
-}

module Main where

import Regtfa as R
import Fatrex as F

main :: IO () 
main = do
    putStrLn "Choose a number from the following options."
    putStrLn "1 ) RegEx -> FA transformation."
    putStrLn "2 ) FA    -> RegEx transformation."
    putStr "Choose the action: "
    option <- getLine
    putStrLn ("You chose the option " ++ option)
    process option

process :: String -> IO ()
process opt
    | opt == "1" || opt == "2"   = 
        let result = if opt == "1" then R.toFinAuto else F.toRegex
        in case result of 
                Just a -> putStrLn ("The result is " ++ a)
                Nothing -> putStrLn ("An error occured.")
    | otherwise     = error "Non existing action"  

module Main where

import qualified Q1
import qualified Q2
import qualified Q3
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    runWithArgs args

runWithArgs :: [String] -> IO ()
runWithArgs args = case args of 
    ["q1"] -> Q1.main
    ["q2"] -> Q2.main
    ["q3"] -> Q3.main
    _ -> putStrLn "Invalid argument. Please provide a valid question number (q1, q2, q3)"

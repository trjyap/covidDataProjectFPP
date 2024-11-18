module Main where

import Text.CSV
import Data.List (maximumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)

-- Function to extract the state and number of beds from a row
-- Assumes the headers: date,state,beds, where "beds" is the third column
extractStateAndBeds :: Record -> Maybe (String, Int)
extractStateAndBeds row =
  case row of
    (_:state:bedsStr:_) -> case reads bedsStr :: [(Int, String)] of
                             [(beds, "")] -> Just (state, beds)
                             _            -> Nothing
    _                   -> Nothing

-- Main function
main :: IO ()
main = do
  -- Read the CSV file
  csvData <- readFile "hospital.csv"
  
  case parseCSV "hospital.csv" csvData of
    Left err -> putStrLn $ "Error parsing CSV: " ++ show err
    Right records -> do
      -- Extract valid (state, beds) pairs
      let stateBeds = mapMaybe extractStateAndBeds (tail records) -- Skip header row
      -- Find the state with the maximum number of beds
      let maxState = maximumBy (comparing snd) stateBeds
      -- Print the result
      putStrLn $ "State with the highest total hospital beds: " 
              ++ fst maxState ++ " (" ++ show (snd maxState) ++ " beds)"

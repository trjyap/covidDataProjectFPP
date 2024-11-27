module Q1 where

-- Importing necessary modules
import Text.CSV                    -- For reading and parsing CSV files
import Data.List (maximumBy)       -- To find the maximum element in a list based on a comparison
import Data.Maybe (mapMaybe)       -- To handle optional values (i.e., `Maybe` values)
import Data.Ord (comparing)        -- To compare elements based on a specific property

-- Function to extract the state name and the number of hospital beds from a row in the CSV file
-- This function processes each row to return a `Maybe` (optional) result
extractStateAndBeds :: Record -> Maybe (String, Int)
extractStateAndBeds row =
  case row of
    -- Check if the row has at least three fields: the second being the state and the third being beds
    (_:state:bedsStr:_) ->
      -- Try to convert the `bedsStr` (a string) into an integer using `reads`
      case reads bedsStr :: [(Int, String)] of
        -- If successful, return the state and the number of beds as a `Just` value
        [(beds, "")] -> Just (state, beds)
        -- If conversion fails, return `Nothing` (invalid data)
        _            -> Nothing
    -- If the row doesn't match the expected structure, return `Nothing`
    _                   -> Nothing

-- Main function to process the CSV file and find the state with the highest number of beds
main :: IO ()
main =
  -- Read the CSV file and bind its content to `csvData`
  readFile "hospital.csv" >>= \csvData ->
    case parseCSV "hospital.csv" csvData of
      Left err -> putStrLn $ "Error parsing CSV: " ++ show err
      -- If parsing succeeds, process the data
      Right records ->
        -- Process the rows of the CSV:
        let 
            -- Use `mapMaybe` to extract valid (state, beds) pairs, ignoring invalid rows
            -- `tail records` skips the header row (assumes the first row is a header)
            stateBeds = mapMaybe extractStateAndBeds (tail records)

            -- Find the state with the maximum number of beds
            -- `maximumBy` compares the second element (number of beds) in each pair
            maxState = maximumBy (comparing snd) stateBeds
        in 
            -- Print the result: the state name and its total number of hospital beds
            putStrLn $ "State with the highest total hospital beds: " 
                    ++ fst maxState ++ " (" ++ show (snd maxState) ++ " beds)"

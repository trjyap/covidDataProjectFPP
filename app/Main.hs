module Main where

import Text.CSV
import Data.List (maximumBy, groupBy, sortBy, foldl', elemIndex)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Ord (comparing)
import Data.Function (on)
import Text.Printf (printf)

-- Prompts user to select a question
getUserChoice :: IO Int
getUserChoice =
    putStrLn "Enter question number (1, 2, 3): " >>
    putStrLn "1. Q1" >>
    putStrLn "2. Q2" >>
    putStrLn "3. Q3" >>
    putStrLn "4. Exit" >>
    getLine >>= \input -> return (read input)

-- Processes the user's choice and runs the corresponding question until exit is chosen
processUserChoice :: Int -> IO ()
processUserChoice choice = case choice of
    1 -> q1 >> getUserChoice >>= processUserChoice
    2 -> q2 >> getUserChoice >>= processUserChoice
    3 -> q3 >> getUserChoice >>= processUserChoice
    4 -> putStrLn "Exiting program"
    _ -> putStrLn "Invalid choice. Please provide a valid question number (1, 2, 3) or 4 to exit"

-- Main function to run the program
main :: IO ()
main = getUserChoice >>= processUserChoice

-- Question 1 ==============================================
-- Processes each row to return a state name and number of hospital beds if present
extractStateAndBeds :: Record -> Maybe (String, Int)
extractStateAndBeds row =
  case row of
    -- Checks if the row has at least three fields: 2nd being the state and 3rd being beds
    (_:state:bedsStr:_) ->
      -- Converts `bedsStr` (a string) into an integer using `reads`
      case reads bedsStr :: [(Int, String)] of
        [(beds, "")] -> Just (state, beds)
        _            -> Nothing
    _                   -> Nothing

q1 :: IO ()
q1 =
  readFile "hospital.csv" >>= \csvData ->
    case parseCSV "hospital.csv" csvData of
      Left err -> putStrLn $ "Error parsing CSV: " ++ show err
      Right records ->
        let
            -- `mapMaybe` to extracts valid (state, beds) pairs, ignoring invalid rows
            stateBeds = mapMaybe extractStateAndBeds (tail records) -- skips the header row
            -- `maximumBy` compares the second element `beds` in each pair to get highest total beds
            maxState = maximumBy (comparing snd) stateBeds
        in
            -- `uncurry` splits `maxState` to work on pairs correspondingly (state name and total beds)
            uncurry (printf "State with the highest total hospital beds: %s (%d beds)\n") maxState >>
            putStrLn ""


-- Question 2 ==============================================
-- Gets the header index of a column
findIndex :: String -> Record -> Maybe Int
findIndex = elemIndex

-- Parses an integer from a string, with a default value of 0
parseInt :: String -> Int
parseInt s = fromMaybe 0 (readMaybe s)

-- Ensures safety when reading an integer
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(val, "")] -> Just val
  _           -> Nothing

-- Processes each record and accumulate the totals
processRecord :: Int -> Int -> (Int, Int) -> Record -> (Int, Int)
processRecord bedsIndex bedsCovidIndex (totalBeds, totalBedsCovid) record =
  if length record > max bedsIndex bedsCovidIndex then
    let beds = parseInt (record !! bedsIndex)
        bedsCovid = parseInt (record !! bedsCovidIndex)
    in (totalBeds + beds, totalBedsCovid + bedsCovid)
  else
    (totalBeds, totalBedsCovid)


q2 :: IO ()
q2 =
    readFile "hospital.csv" >>= \csvData ->
  case parseCSV "hospital.csv" csvData of
    Left err -> putStrLn $ "Error when parsing CSV: " ++ show err
    Right [] -> putStrLn "Error: The CSV file is empty"
    Right (header:records) ->
      case (findIndex "beds" header, findIndex "beds_covid" header) of -- Specifies beds and beds_covid columns
        (Just bedsIndex, Just bedsCovidIndex) ->
          let (totalBeds, totalBedsCovid) = foldl' (processRecord bedsIndex bedsCovidIndex) (0, 0) records
              ratio :: Maybe Double
              ratio = if totalBeds == 0 then Nothing else Just (fromIntegral totalBedsCovid / fromIntegral totalBeds)
          in putStrLn ("Total beds: " ++ show totalBeds ++ "\n" ++
                      "Total beds for COVID-19: " ++ show totalBedsCovid ++ "\n" ++
                      "Ratio of beds for COVID-19 to total beds: " ++ maybe "N/A" (printf "%.4f") ratio ++ "\n") -- 4 decimal places for ratio and N/A if totalBeds is 0
        _ -> putStrLn "Error: Missing column(s) in the CSV file: beds, beds_covid"


-- Question 3 ==============================================
-- Prevents index out of bounds error
(!!?) :: [a] -> Int -> Maybe a
list !!? i
  | i >= 0 && i < length list = Just (list !! i)
  | otherwise = Nothing

-- Processes each row to return state name, suspected, covid positive, and non-covid patients if present
extractCategoryData :: [String] -> Record -> Maybe (String, Int, Int, Int)
extractCategoryData headers row =
    -- Extracts the indices of the columns
  let stateIndex = elemIndex "state" headers
      suspectedIndex = elemIndex "admitted_pui" headers
      covidPositiveIndex = elemIndex "admitted_covid" headers
      nonCovidIndex = elemIndex "hosp_noncovid" headers
  in case (stateIndex, suspectedIndex, covidPositiveIndex, nonCovidIndex) of
       (Just si, Just susi, Just cpi, Just nci) ->
         case (row !!? si, row !!? susi, row !!? cpi, row !!? nci) of
           (Just state, Just suspectedStr, Just positiveStr, Just nonCovidStr) ->
            -- Converts the strings to integers
             case (reads suspectedStr, reads positiveStr, reads nonCovidStr) of
               ([(suspected, "")], [(covidPositive, "")], [(nonCovid, "")]) ->
                 Just (state, suspected, covidPositive, nonCovid)
               _ -> Nothing
           _ -> Nothing
       _ -> Nothing

-- Calculates the averages for each state
calculateAverages :: [(String, Int, Int, Int)] -> [(String, (Double, Double, Double))]
calculateAverages records =
    -- Groups the records by state
  let grouped = groupBy ((==) `on` fst) . sortBy (compare `on` fst) $ map (\(state, s, c, n) -> (state, (s, c, n))) records
  in map calculateStateAverage grouped
  where
    -- Helper function for state average calculation
    calculateStateAverage recordsForState =
      let state = fst (head recordsForState)
            -- Accumulates the totals row by row
          totals = foldr (\(_, (s, c, n)) (ts, tc, tn, cnt) ->
                          (ts + s, tc + c, tn + n, cnt + 1))
                         (0, 0 :: Int, 0 :: Int, 0 :: Int)
                         recordsForState
          (totalSuspected, totalCovidPositive, totalNonCovid, count) = totals
      in (state,
          (fromIntegral totalSuspected / fromIntegral count,
           fromIntegral totalCovidPositive / fromIntegral count,
           fromIntegral totalNonCovid / fromIntegral count))

q3 :: IO ()
q3 =
    readFile "hospital.csv" >>= \csvData ->
    case parseCSV "hospital.csv" csvData of
      Left err -> putStrLn $ "Error parsing CSV: " ++ show err
      Right records ->
        case records of
          [] -> putStrLn "Error: Empty CSV file"
          (header:rows) ->
            -- Extracts valid rows only with `mapMaybe` (state, suspected, covidPositive, nonCovid)
            let categoryData = mapMaybe (extractCategoryData header) rows -- Uses the header row to determine column indices
                averages = calculateAverages categoryData
            -- Maps monadically with `mapM_` to print the averages
            in mapM_
                 (\(state, (avgS, avgC, avgN)) ->
                    printf "State: %s, Avg Suspected: %.2f, Avg COVID Positive: %.2f, Avg Non-COVID: %.2f\n"
                      state avgS avgC avgN)
                 averages >>
                 putStrLn ""

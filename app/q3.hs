module Q3 where

import Text.CSV (parseCSV, Record)
import Data.List (groupBy, sortBy, elemIndex)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Text.Printf (printf)

-- Helper function for safe list indexing
(!!?) :: [a] -> Int -> Maybe a
list !!? i
  | i >= 0 && i < length list = Just (list !! i)
  | otherwise = Nothing

-- Function to extract a record into (state, suspected, covidPositive, nonCovid)
extractCategoryData :: [String] -> Record -> Maybe (String, Int, Int, Int)
extractCategoryData headers row =
  let stateIndex = elemIndex "state" headers
      suspectedIndex = elemIndex "admitted_pui" headers
      covidPositiveIndex = elemIndex "admitted_covid" headers
      nonCovidIndex = elemIndex "hosp_noncovid" headers
  in case (stateIndex, suspectedIndex, covidPositiveIndex, nonCovidIndex) of
       (Just si, Just susi, Just cpi, Just nci) ->
         case (row !!? si, row !!? susi, row !!? cpi, row !!? nci) of
           (Just state, Just suspectedStr, Just positiveStr, Just nonCovidStr) ->
             case (reads suspectedStr, reads positiveStr, reads nonCovidStr) of
               ([(suspected, "")], [(covidPositive, "")], [(nonCovid, "")]) ->
                 Just (state, suspected, covidPositive, nonCovid)
               _ -> Nothing
           _ -> Nothing
       _ -> Nothing

-- Function to calculate averages per state
calculateAverages :: [(String, Int, Int, Int)] -> [(String, (Double, Double, Double))]
calculateAverages records =
  let grouped = groupBy ((==) `on` fst) . sortBy (compare `on` fst) $ map (\(state, s, c, n) -> (state, (s, c, n))) records
  in map calculateStateAverage grouped
  where
    calculateStateAverage recordsForState =
      let state = fst (head recordsForState)
          totals = foldr (\(_, (s, c, n)) (ts, tc, tn, count) ->
                          (ts + s, tc + c, tn + n, count + 1))
                         (0, 0, 0, 0)
                         recordsForState
          (totalSuspected, totalCovidPositive, totalNonCovid, count) = totals
      in (state,
          (fromIntegral totalSuspected / fromIntegral count,
           fromIntegral totalCovidPositive / fromIntegral count,
           fromIntegral totalNonCovid / fromIntegral count))

-- Main function
main :: IO ()
main =
  readFile "hospital.csv" >>= \csvData ->
    case parseCSV "hospital.csv" csvData of
      Left err -> putStrLn $ "Error parsing CSV: " ++ show err
      Right records ->
        case records of
          [] -> putStrLn "Error: Empty CSV file"
          (header:rows) ->
            let categoryData = mapMaybe (extractCategoryData header) rows -- Use the header row to determine column indices
                averages = calculateAverages categoryData
            in mapM_
                 (\(state, (avgS, avgC, avgN)) ->
                    printf "State: %s, Avg Suspected: %.2f, Avg COVID Positive: %.2f, Avg Non-COVID: %.2f\n"
                      state avgS avgC avgN)
                 averages

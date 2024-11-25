module Q3 where

import Text.CSV
import Data.List (groupBy, sortBy)
import Data.Function (on)
import Data.Maybe (mapMaybe)

-- Function to extract a record into (state, suspected, covidPositive, nonCovid)
extractCategoryData :: Record -> Maybe (String, Int, Int, Int)
extractCategoryData row =
  case row of
    (_:state:suspectedStr:positiveStr:nonCovidStr:_) ->
      case (reads suspectedStr, reads positiveStr, reads nonCovidStr) of
        ([(suspected, "")], [(covidPositive, "")], [(nonCovid, "")]) ->
          Just (state, suspected, covidPositive, nonCovid)
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
        let categoryData = mapMaybe extractCategoryData (tail records) -- Skip header row
            averages = calculateAverages categoryData
        in mapM_
             (\(state, (avgS, avgC, avgN)) ->
                putStrLn $ "State: " ++ state
                          ++ ", Avg Suspected: " ++ show avgS
                          ++ ", Avg COVID Positive: " ++ show avgC
                          ++ ", Avg Non-COVID: " ++ show avgN)
             averages

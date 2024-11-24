module Q2 where
  
import Text.CSV
import System.IO (readFile)
import Data.Maybe (fromMaybe)
import Data.List (foldl')

-- Defines the column indices
dateIndex, stateIndex, bedsIndex, bedsCovidIndex :: Int
dateIndex = 0
stateIndex = 1
bedsIndex = 2
bedsCovidIndex = 3

-- Parses an integer from a string, with a default value of 0
parseInt :: String -> Int
parseInt s = fromMaybe 0 (readMaybe s)

-- Ensures safety when reading an integer
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(val, "")] -> Just val
  _           -> Nothing

main :: IO ()
main =
  readFile "hospital.csv" >>= \csvData ->
  case parseCSV "hospital.csv" csvData of
    Left err -> putStrLn $ "Error parsing CSV: " ++ show err
    Right records ->
      let (totalBeds, totalBedsCovid) = foldl' processRecord (0, 0) (tail records) -- Skip header
          ratio = if totalBeds == 0 then Nothing else Just (fromIntegral totalBedsCovid / fromIntegral totalBeds)
      in putStrLn ("Total beds: " ++ show totalBeds ++ "\n" ++
                   "Total beds for COVID-19: " ++ show totalBedsCovid ++ "\n" ++
                   "Ratio of beds for COVID-19 to total beds: " ++ show ratio)

-- Processes each record and accumulate the totals
processRecord :: (Int, Int) -> Record -> (Int, Int)
processRecord (totalBeds, totalBedsCovid) record =
  if length record > bedsCovidIndex then
    let beds = parseInt (record !! bedsIndex)
        bedsCovid = parseInt (record !! bedsCovidIndex)
    in (totalBeds + beds, totalBedsCovid + bedsCovid)
  else
    (totalBeds, totalBedsCovid)
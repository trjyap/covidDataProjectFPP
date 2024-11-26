module Q2 where
  
import Text.CSV 
import System.IO ()
import Data.Maybe (fromMaybe)
import Data.List (foldl', elemIndex)
import Text.Printf (printf)

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

-- Reads the CSV file and processes the records for output
main :: IO ()
main =
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
                      "Ratio of beds for COVID-19 to total beds: " ++ maybe "N/A" (printf "%.4f") ratio) -- 4 decimal places for ratio and N/A if totalBeds is 0
        _ -> putStrLn "Error: Missing column(s) in the CSV file: beds, beds_covid"
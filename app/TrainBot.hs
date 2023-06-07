import Wordle
import Data.Time

main = do
  fileContent <- readWordsFile
  let allWords = fiveLettersWords fileContent
  startTime <- getCurrentTime
  progressivePlayAll allWords 0 0
  endTime <- getCurrentTime
  putStrLn $ "Execution Time: " ++ show (diffUTCTime endTime startTime)

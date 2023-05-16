import Wordle
import Data.Time

main = do
  fileContent <- readWordsFile
  let allWords = fiveLettersWords fileContent
  startTime <- getCurrentTime
  let count = autoplayAll allWords
  print $ fromIntegral count / fromIntegral (length allWords)
  endTime <- getCurrentTime
  let diff = diffUTCTime endTime startTime
  putStrLn $ "Execution Time: " ++ show diff

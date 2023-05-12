import Wordle

printStats = do
  fileContent <- readWordsFile
  putStr $ stats $ fiveLettersWords fileContent


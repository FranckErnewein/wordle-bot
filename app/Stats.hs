import Wordle

main = do
  fileContent <- readWordsFile
  putStr $ stats $ fiveLettersWords fileContent


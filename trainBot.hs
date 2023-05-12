import Wordle

main = do
  fileContent <- readWordsFile
  let allWords = fiveLettersWords fileContent
  let count = autoplayAll allWords allWords 0
  print $ fromIntegral count / fromIntegral (length allWords)

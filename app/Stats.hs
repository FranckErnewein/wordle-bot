import Wordle
import Data.List
import Data.Function

main = do
  fileContent <- readWordsFile
  let allWords = fiveLettersWords fileContent
  putStrLn "all words by score:"
  print $ sortBy (flip compare `on` fst) $ zip (map (scoreWord allWords) allWords) allWords

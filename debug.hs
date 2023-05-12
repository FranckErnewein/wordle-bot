import Wordle
import Data.List
import Data.Function

main = do
  fileContent <- readWordsFile
  let allWords = fiveLettersWords fileContent
  let t = parseTry "RAIES" "10021"
  let filteredWords = filterWords allWords [t]
  let popLetters = popularLetters filteredWords
  print popLetters 
  print filteredWords
  let scores = map (scoreWord popLetters . nub) filteredWords 
  print $ sortBy (flip compare `on` fst) (zip scores filteredWords)

  

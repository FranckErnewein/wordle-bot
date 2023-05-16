import Wordle
import Data.List
import Data.Function
import Control.Monad


main = forever $ do
  fileContent <- readWordsFile
  let allWords = fiveLettersWords fileContent
  -- print $ sortBy (flip compare `on` fst) $ zip (map (scoreWord allWords) allWords) allWords
  w1 <- getLine
  w2 <- getLine
  putStrLn $ map displayLetterGuess (guess w1 w2)
  -- putStrLn $ map displayLetterGuess (guess "MOLLE" "ECOLE")

import Wordle
import Data.List
import Data.Function
import Control.Monad


main = do
  fileContent <- readWordsFile
  let allWords = fiveLettersWords fileContent
  putStrLn $ displayTries $ autoplay allWords (allWords !! 0) []
  -- putStrLn $ displayTries $ autoplay allWords (allWords !! 1) []
  -- print $ sortBy (flip compare `on` fst) $ zip (map (scoreWord allWords) allWords) allWords
  
  let wordToFind = "CETTE"
  print $ idealWord allWords []
  let t0 = [guess "RAIES" wordToFind]
  putStrLn $ displayTries t0
  print $ idealWord allWords t0 

  let t1 = guess "COULE" wordToFind:t0
  putStrLn $ displayTries t1
  print $ idealWord allWords t1 

  let t2 = guess "CHECK" wordToFind:t1
  putStrLn $ displayTries t2
  print $ filterWords allWords t2 
  print $ idealWord allWords t2 

  -- let t3 = guess "COMBE" wordToFind:t2
  -- putStrLn $ displayTries t3
  -- print $ idealWord allWords t3 

  -- let t4 = guess "COMME" wordToFind:t3
  -- putStrLn $ displayTries t4
  -- print $ idealWord allWords t4 

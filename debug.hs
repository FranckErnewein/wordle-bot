import Wordle
import Data.List
import Data.Function
import Control.Monad


main = do
  fileContent <- readWordsFile
  let allWords = fiveLettersWords fileContent
  let bigFail = ["RARES","SALES","HACHE","SEXES","CASES","SITES","SURES","NAINS","RASES","SAINS","NAINE","MAMES","SOUES","HALES","BITES","DARDE","ZELES","RAPES","TELES","CILLE","DINES","GILLE","TALES","BELER","RACER","CAKES","OILLE","TOUES","HELEE","HELES","CORES","SORES","TATES","RABES","SASSE","MATTE"]

  let alltries = map (\w -> autoplay allWords w []) bigFail
  putStrLn $ displayTries (alltries !! 1)
  -- print $ sortBy (flip compare `on` fst) $ zip (map (scoreWord allWords) allWords) allWords
  
  -- let wordToFind = "COMME"
  -- print $ idealWord allWords []
  -- let t0 = [guess "RAIES" wordToFind]
  -- putStrLn $ displayTries t0
  -- print $ filterWords allWords t0
  -- print $ idealWord allWords t0

  -- let t1 = guess "LEONE" wordToFind:t0
  -- putStrLn $ displayTries t1
  -- print $ filterWords allWords t1
  -- print $ idealWord allWords t1

  -- let t2 = guess "CHECK" wordToFind:t1
  -- putStrLn $ displayTries t2
  -- print $ filterWords allWords t2
  -- print $ idealWord allWords t2

  -- let t3 = guess "COMBE" wordToFind:t2
  -- putStrLn $ displayTries t3
  -- print $ idealWord allWords t3 

  -- let t4 = guess "COMME" wordToFind:t3
  -- putStrLn $ displayTries t4
  -- print $ idealWord allWords t4

import Wordle
import Data.Char

main = do
  fileContent <- readWordsFile
  putStrLn "What is the word today ?"
  wordToFind <- getLine
  putStrLn "ok, let's play:"
  print $ autoplay (fiveLettersWords fileContent) (map toUpper wordToFind) []
  -- putStrLn $ unlines $ map (map (\(LetterGuess c s) -> c )) (reverse tries)
  -- putStrLn $ "found in " ++ (show . length) tries ++ " tries"
  -- putStrLn $ displayTries tries

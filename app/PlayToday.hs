import Wordle
import Data.Char
import Control.Monad

main = forever $ do
  fileContent <- readWordsFile
  putStrLn "What is the word today ?"
  wordToFind <- getLine
  putStrLn "ok, let's play:"
  let tries = autoplay (fiveLettersWords fileContent) (map toUpper wordToFind) []
  putStrLn $ "found in " ++ (show . length) tries ++ " tries"
  putStrLn $ displayTries tries

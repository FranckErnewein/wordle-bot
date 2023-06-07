import Wordle
import Data.Char
import Data.List
import Data.Function
import Control.Monad
import System.Exit as Exit

checkWordIsInList :: [String] -> String -> IO ()
checkWordIsInList ws w
  | w `elem` ws = putStrLn "ok, let's play:"
  | otherwise = putStrLn (w ++ " is not in the words list") >> main


main = forever $ do
  fileContent <- readWordsFile
  putStrLn "What is the word today ?"
  input <- getLine
  let wordToFind = map toUpper input
  let allWords = fiveLettersWords fileContent
  checkWordIsInList allWords wordToFind

  progressivePlay allWords wordToFind []

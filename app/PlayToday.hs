import Wordle
import Data.Char
import Data.List
import Data.Function
import Control.Monad
import System.Exit as Exit

checkWord :: [String] -> String -> IO ()
checkWord ws w
  | w `elem` ws = putStrLn "ok, let's play:"
  | otherwise = putStrLn (w ++ " is not in the words list") >> main


progressivePlay :: [String] -> String -> Tries -> IO ()
progressivePlay ws w t
  | hasWon t = putStrLn $ "won in " ++ show (length t) ++ " tries."
  | otherwise = print t >> putStrLn (displayAnswer (head stackTry)) >> next
  where stackTry = play t (idealWord ws t) w
        next = progressivePlay ws w stackTry

main = forever $ do
  fileContent <- readWordsFile
  putStrLn "What is the word today ?"
  input <- getLine
  let wordToFind = map toUpper input
  let allWords = fiveLettersWords fileContent
  checkWord allWords wordToFind

  progressivePlay allWords wordToFind []

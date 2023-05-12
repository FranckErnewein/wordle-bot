module Wordle where

import Control.Monad
import Data.Function
import Data.List
import Data.Char
import qualified Data.Map as Map
import System.IO

data LetterStatus = GoodPlace | BadPlace | BadLetter deriving (Show, Eq, Read)
data LetterGuess = LetterGuess Char LetterStatus deriving (Eq, Show, Read)

type GameAnswer = [LetterGuess]
type Tries = [[LetterGuess]]

lettersCount :: Map.Map Char Int
lettersCount = Map.fromList (map (, 0) ['A'..'Z'])

addLetter :: Map.Map Char Int -> Char -> Map.Map Char Int
addLetter m c = Map.insertWith (+) c 1 m

countLetter :: String -> Map.Map Char Int -> Map.Map Char Int
countLetter [] m = m
countLetter xs m = foldl addLetter m xs

aggregateLetters :: String -> Map.Map Char Int
aggregateLetters xs = countLetter xs lettersCount

popularLetters :: [String] -> Map.Map Char Int
popularLetters = aggregateLetters . concat

popularLettersAt :: [String] -> Int -> Map.Map Char Int
popularLettersAt w x = aggregateLetters $ map (!! x) w  

lettersWithStatus :: Tries -> LetterStatus -> String
lettersWithStatus tries status = map (\(LetterGuess c _) -> c) $ filter (\(LetterGuess _ s) -> s == status) (concat tries)


checkGuessChar :: String -> (LetterGuess, Int) -> Bool
checkGuessChar word (LetterGuess cg status, i) 
  | status == GoodPlace = word !! i == cg
  | status == BadPlace = word !! i /= cg && cg `elem` word
  | status == BadLetter = cg `notElem` word

checkGuess :: (GameAnswer -> String -> Bool)
checkGuess a w =
  let zipped = zip a [0..]
  in all (checkGuessChar w) zipped


wordWithPopularLetter :: [(Char, Int)] -> Int -> [String] -> String
wordWithPopularLetter letters limit ws
  | null filtered = wordWithPopularLetter letters (limit - 1) ws 
  | otherwise = head filtered
  where 
    sortedTopLetters = take limit $ sort $ map fst letters
    filtered = filter (\w -> sortedTopLetters `intersect` take limit (sort w) == sortedTopLetters) ws

filterWords :: [String] -> Tries -> [String]
-- filterWords ws [] = ws
-- filterWords ws (t:xt) = filterWords (filter (checkGuess t) ws) xt
filterWords = foldl (\ ws t -> filter (checkGuess t) ws)

scoreWord :: Map.Map Char Int -> String -> Int
scoreWord m [] = 0
scoreWord m (l:w) = scoreWord m w + m Map.! l

idealWord :: [String] -> Tries -> String
idealWord ws ts =
  let filtered = filterWords ws ts
      letters = popularLetters filtered
      scores = map (scoreWord letters . nub) filtered
      scored = zip scores filtered
      ordered = sortBy (flip compare `on` fst) scored
  in snd $ head ordered

displayTupleList :: [(Char, Int)] -> String
displayTupleList [] = ""
displayTupleList ((c, x):xs) = "\n - " ++ c:  ":" ++ show x ++ displayTupleList xs

fiveLettersWords :: String -> [String] 
fiveLettersWords = filter (\x -> length x == 5) . lines

checkLetter :: String -> (Char, Char) -> LetterGuess
checkLetter wordSolution (letterTried, letterSolution)
  | letterTried == letterSolution = LetterGuess letterTried GoodPlace
  | letterTried `elem` wordSolution = LetterGuess letterTried BadPlace 
  | otherwise = LetterGuess letterTried BadLetter 

guess :: String -> String -> GameAnswer
guess wordTried wordToFind = zipWith (curry (checkLetter wordToFind)) wordTried wordToFind 

play :: Tries -> String -> String -> Tries
play tries wordTried wordToFind = guess wordTried wordToFind : tries

hasWon :: Tries -> Bool
hasWon tries = all (\(LetterGuess _ s) -> s == GoodPlace) (head tries)

autoplay :: [String] -> String -> Tries -> Tries
autoplay allwords wordToFind tries   
  | hasWon newTries = newTries
  | otherwise = autoplay allwords wordToFind newTries
  where newTries = play tries (idealWord allwords tries) wordToFind

autoplayAll :: [String] -> [String] -> Int -> Int
autoplayAll allwords [] x = x
autoplayAll allwords (w:ws) x = autoplayAll allwords ws (x + length (autoplay allwords w []))

stats :: [String] ->  String
stats w = unlines $ [
    "total words analysed: " ++ show (length w), " "]
    -- "letters sorted by popularity:", displayTupleList $ popularLetters w, " "
  -- ] ++ map (\i -> "position " ++ show (i + 1) ++ ": " ++ displayTupleList (take 5 (popularLettersAt w i)) ++ "\n") [0..4]
  


displayLetterGuess :: LetterGuess -> Char
displayLetterGuess (LetterGuess c s)
  | s == BadLetter = '⬛'
  | s == GoodPlace = '🟩'
  | s == BadPlace = '🟨'

displayTries :: Tries -> String
displayTries tries  
  | hasWon tries = unlines $ map (map displayLetterGuess) (reverse tries)
  | otherwise = "----"


readWordsFile :: IO String
readWordsFile = do
  handle <- openFile "5letterswords.txt" ReadMode
  hGetContents handle

-- WIP

parseAnswer :: Char -> LetterStatus
parseAnswer a  
    | a == '0' = BadLetter
    | a == '1' = BadPlace
    | a == '2' = GoodPlace

parseTry :: String -> String -> GameAnswer
parseTry word answer = map (\(c, a) -> (LetterGuess c (parseAnswer a))) (zip word answer)

module Wordle where

import Control.Monad
import Data.Function
import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Map as Map
import System.IO

data LetterStatus = GoodPlace | BadPlace | BadLetter deriving (Show, Eq, Read)
data LetterGuess = LetterGuess Char LetterStatus deriving (Eq, Show, Read)

type GameAnswer = [LetterGuess]
type Tries = [[LetterGuess]]

-- utils
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- Letter Stats
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
-- Letter Stats End

lettersWithStatus :: Tries -> LetterStatus -> String
lettersWithStatus tries status = map (\(LetterGuess c _) -> c) $ filter (\(LetterGuess _ s) -> s == status) (concat tries)

checkGuessChar :: String -> (LetterGuess, Int) -> Bool
checkGuessChar word (LetterGuess cg status, i) 
  | status == GoodPlace = word !! i == cg
  | status == BadPlace = word !! i /= cg && cg `elem` word
  | status == BadLetter = cg `notElem` word

checkGuess :: GameAnswer -> String -> Bool
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
-- filterWords = foldl (\ ws t -> filter (checkGuess t) ws)
filterWords ws [] = ws
filterWords ws (t:ts) = filterWords (filter (checkGuess t) ws) ts


statusScore :: LetterStatus -> Int
statusScore GoodPlace = 2
statusScore BadPlace = 1
statusScore BadLetter = 0

answerToScore :: GameAnswer -> Int
answerToScore = foldl (\i (LetterGuess _ s) -> i + statusScore s) 0

scoreWord :: [String] -> String -> Int
scoreWord [] w = 0
scoreWord (x:xs) w = scoreWord xs w + answerToScore (guess x w)

idealWord :: [String] -> Tries -> String
idealWord _ [] = "RAIES"
idealWord ws ts =
  let filtered = filterWords ws ts
      scored = map (\w -> (w, scoreWord filtered w)) filtered
      ordered = sortBy (flip compare `on` snd) scored
  in fst $ head ordered

fiveLettersWords :: String -> [String] 
fiveLettersWords = filter (\x -> length x == 5) . lines

checkLetter :: String -> (Char, Char) -> LetterGuess
checkLetter wordSolution (letterTried, letterSolution)
  | letterTried == letterSolution = LetterGuess letterTried GoodPlace
  | letterTried `elem` wordSolution = LetterGuess letterTried BadPlace 
  | otherwise = LetterGuess letterTried BadLetter 

elemAvoid :: (Eq a) => a -> [a] -> [Int] -> Int
-- elemAvoid c ws [] = fromMaybe 1000 (c `elemIndex` ws)
elemAvoid c ws banned =fromMaybe 1000 $ find (`notElem` banned) (c `elemIndices` ws)

iterateLetters :: String -> (GameAnswer, Int, [Int]) -> Char -> (GameAnswer, Int, [Int])
iterateLetters solution (answer, i, used) c
    | idx == 1000 || idx `elem` used = (answer ++ [LetterGuess c BadLetter], i+1, used)
    | idx == i = (answer ++ [LetterGuess c GoodPlace], i+1, used ++ [idx])
    | c `elem` solution = (answer ++ [LetterGuess c BadPlace], i+1, used ++ [idx])
    | otherwise = (answer ++ [LetterGuess c BadLetter], i+1, used)
    where idx = elemAvoid c solution used

guess :: String -> String -> GameAnswer
guess guessWord solution = fst3 $ foldl (iterateLetters solution) ([], 0, []) guessWord

play :: Tries -> String -> String -> Tries
play tries wordTried wordToFind = guess wordTried wordToFind : tries

hasWon :: Tries -> Bool
hasWon tries = all (\(LetterGuess _ s) -> s == GoodPlace) (head tries)

autoplay :: [String] -> String -> Tries -> Tries
autoplay allwords wordToFind tries   
  | hasWon newTries = newTries
  | otherwise = autoplay allwords wordToFind newTries
  where newTries = play tries (idealWord allwords tries) wordToFind

autoplayAll :: [String] -> Int
autoplayAll ws = foldl (\x w -> x + length (autoplay ws w [])) 0 ws

displayTupleList :: [(Char, Int)] -> String
displayTupleList [] = ""
displayTupleList ((c, x):xs) = "\n - " ++ c:  ":" ++ show x ++ displayTupleList xs

displayLetterGuess :: LetterGuess -> Char
displayLetterGuess (LetterGuess c s)
  | s == BadLetter = '⬛'
  | s == GoodPlace = '🟩'
  | s == BadPlace = '🟨'

displayTries :: Tries -> String
displayTries tries = unlines $ map (map displayLetterGuess) (reverse tries)

readWordsFile :: IO String
readWordsFile = do
  handle <- openFile "5letterswords.txt" ReadMode
  hGetContents handle


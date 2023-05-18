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


rmLetter :: String -> Int -> String
rmLetter word i
  | i < length word = word
  | otherwise = take i word ++ "_" ++ drop (i+1) word

rmNextLetter :: String -> Char -> String
rmNextLetter w c = rmLetter w (fromMaybe 1000 (c `elemIndex` w))

iterateGuess :: (Bool, Int, String) -> GameAnswer -> Bool
iterateGuess (False, _, _) _ = False
iterateGuess (True, _, _) [] = True
iterateGuess (True, i, word) ((LetterGuess c s):lgs)
  | s == GoodPlace = iterateGuess (word !! i == c, i+1, rmLetter word i) lgs
  | s == BadPlace = iterateGuess (word !! i /= c && c `elem` word, i+1, rmLetter word (fromMaybe 1000 (c `elemIndex` word))) lgs
  | s == BadLetter = iterateGuess (c `notElem` word, i+1, word) lgs

checkGuess :: GameAnswer -> String -> Bool
checkGuess answer w = iterateGuess (True, 0, w) answer

filterWords :: [String] -> Tries -> [String]
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
elemAvoid c ws banned = fromMaybe 1000 $ find (`notElem` banned) (c `elemIndices` ws)

iterateLetters :: String -> String -> Int -> GameAnswer -> GameAnswer
iterateLetters [] _ _ answer = answer 
iterateLetters (c:w) solution i answer 
  | (solution !! i) == c = iterateLetters w (rmLetter solution i) (i+1) (answer++[LetterGuess c GoodPlace])
  | c `elem` solution = iterateLetters w (rmNextLetter solution c) (i+1) (answer++[LetterGuess c BadPlace])
  | c `notElem` solution = iterateLetters w solution (i+1) (answer++[LetterGuess c BadLetter])



guess :: String -> String -> GameAnswer
guess guessWord solution = iterateLetters guessWord solution 0 []

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

displayAnswer :: GameAnswer -> String
displayAnswer a = map (\(LetterGuess c _) -> c) a ++ " " ++ map displayLetterGuess a 

displayTries :: Tries -> String
displayTries tries = unlines $ map displayAnswer (reverse tries)

readWordsFile :: IO String
readWordsFile = do
  handle <- openFile "5letterswords.txt" ReadMode
  hGetContents handle


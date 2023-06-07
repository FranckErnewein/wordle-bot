{-# LANGUAGE TupleSections #-}
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

-- accessor
getLetterGuessChar (LetterGuess c _) = c
getLetterGuessStatus (LetterGuess _ s) = s

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
lettersWithStatus tries status = map getLetterGuessChar $ filter (\(LetterGuess _ s) -> s == status) (concat tries)


rmLetter :: String -> Int -> String
rmLetter word i
  | i >= length word = word
  | otherwise = take i word ++ "_" ++ drop (i+1) word

rmNextLetter :: String -> Char -> String
rmNextLetter w c = rmLetter w (fromMaybe 1000 (c `elemIndex` w))

checkGoodPlace :: GameAnswer -> String -> Int -> String
checkGoodPlace _ [] _ = []
checkGoodPlace [] word i = word
checkGoodPlace (LetterGuess c s:lgs) word i
  | s == GoodPlace && word !! i == c = checkGoodPlace lgs (rmLetter word i) (i+1)
  | s == GoodPlace = []
  | otherwise = checkGoodPlace lgs word (i+1)

checkBadPlace :: GameAnswer -> String -> Int -> String
checkBadPlace _ [] _ = []
checkBadPlace [] word _ = word
checkBadPlace (LetterGuess c s:lgs) word i
  | s == BadPlace && word !! i /= c && c `elem` word = checkBadPlace lgs (rmNextLetter word c) (i+1)
  | s == BadPlace = []
  | otherwise = checkBadPlace lgs word (i+1)

checkBadLetter :: GameAnswer -> String -> Bool
checkBadLetter _ [] = False
checkBadLetter answer word = all ((`notElem` word) . getLetterGuessChar) $ filter ((==BadLetter) . getLetterGuessStatus) answer

checkWord :: GameAnswer -> String -> Bool
checkWord answer word = checkBadLetter answer (checkBadPlace answer (checkGoodPlace answer word 0) 0)

filterOnAnswer :: GameAnswer -> [String] -> [String]
filterOnAnswer answer = filter (checkWord answer)

filterOnTries :: Tries -> [String] -> [String]
filterOnTries ts ws = foldl (flip filterOnAnswer) ws ts

statusScore :: LetterStatus -> Int
statusScore GoodPlace = 2
statusScore BadPlace = 1
statusScore BadLetter = 0

answerToScore :: GameAnswer -> Int
answerToScore = foldl (\i lg -> i + (statusScore . getLetterGuessStatus) lg) 0

scoreWord :: [String] -> String -> Int
scoreWord [] w = 0
scoreWord (x:xs) w = scoreWord xs w + answerToScore (guess x w)

idealWord :: [String] -> Tries -> String
idealWord _ [] = "RAIES"
idealWord ws ts =
  let filtered = filterOnTries ts ws
      scored = map (\w -> (w, scoreWord filtered w)) filtered
      ordered = sortBy (flip compare `on` snd) scored
  in fst $ head ordered

fiveLettersWords :: String -> [String] 
fiveLettersWords = filter (\x -> length x == 5) . lines

goodPlaceAt :: String -> String -> [Int]
goodPlaceAt guessWord solution = map fst3 $ filter (\(i, g, s) -> g == s) $ zip3 [0..] guessWord solution

badPlaceAt :: String -> String -> [Int]
badPlaceAt guessWord solution =
  let f [] _ _ = []
      f (c:w) s i
        | s !! i /= c && c `elem` s = i:f w (rmNextLetter s c) (i+1)
        | otherwise = f w s (i+1)
  in f guessWord solution 0

guess :: String -> String -> GameAnswer
guess guessWord solution =
  let gp = goodPlaceAt guessWord solution
      bp = badPlaceAt (foldl rmLetter guessWord gp) (foldl rmLetter solution gp)
      f (c, i)
        | i `elem` gp = LetterGuess c GoodPlace
        | i `elem` bp = LetterGuess c BadPlace
        | otherwise = LetterGuess c BadLetter
  in map f (zip guessWord [0..])

play :: Tries -> String -> String -> Tries
play tries wordTried wordToFind = guess wordTried wordToFind : tries

hasWon :: Tries -> Bool
hasWon [] =  False
hasWon tries = all ((== GoodPlace) . getLetterGuessStatus) (head tries)

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
displayAnswer a = map getLetterGuessChar a ++ " " ++ map displayLetterGuess a

displayTries :: Tries -> String
displayTries tries = unlines $ map displayAnswer (reverse tries)

parseStatus :: Char -> LetterStatus
parseStatus '2' = GoodPlace
parseStatus '1' = BadPlace
parseStatus '0' = BadLetter

parseAnswer :: String -> GameAnswer
parseAnswer input = map (\ (c:s:rest) -> LetterGuess c (parseStatus s)) $ words input

readWordsFile :: IO String
readWordsFile = do
  handle <- openFile "5letterswords.txt" ReadMode
  hGetContents handle

progressivePlay :: [String] -> String -> Tries -> IO Int
progressivePlay ws w t
  | hasWon t = return (length t)
  | otherwise = putStrLn (displayAnswer (head stackTry)) >> next
  where stackTry = play t (idealWord ws t) w
        next = progressivePlay ws w stackTry

progressivePlayAll :: [String] -> Int -> Int -> IO ()
progressivePlayAll ws count i
  | i == length ws = print $ fromIntegral count / fromIntegral i
  | otherwise = do
    putStrLn $ "play " ++ (ws !! i) ++ " (" ++ show (i+1) ++ "/" ++ show (length ws) ++ ")"
    t <- progressivePlay ws (ws !! i) []
    putStr $ if t > 5 then replicate (6 - t) '\n' else "\n"
    progressivePlayAll ws (count + t) (i+1)

module Main where

import Test.HUnit
import Wordle
import qualified System.Exit as Exit

tests :: Test
tests = TestList [ 
    TestCase (assertEqual "hasWon: empty tries" False (hasWon []))
  , TestCase (assertEqual "hasWon: bad try" False (hasWon [[LetterGuess 'a' BadLetter, LetterGuess 'b' GoodPlace]]))
  , TestCase (assertEqual "hasWon: good try" True (hasWon [[LetterGuess 'a' GoodPlace, LetterGuess 'b' GoodPlace]]))

  , TestCase (assertEqual "rmLetter: replace inside" "ME_DE" (rmLetter "MERDE" 2))
  , TestCase (assertEqual "rmLetter: replace last" "MERD_" (rmLetter "MERDE" 4))
  , TestCase (assertEqual "rmLetter: replace first" "_ERDE" (rmLetter "MERDE" 0))

  , TestCase (assertEqual "rmNextLetter: 1 letter" "M_RDE" (rmNextLetter "MERDE" 'E'))
  , TestCase (assertEqual "rmNextLetter: 2 letterz" "M_RD_" (rmNextLetter "M_RDE" 'E'))
  , TestCase (assertEqual "rmNextLetter: 1st letter" "_ERDE" (rmNextLetter "MERDE" 'M'))
  , TestCase (assertEqual "rmNextLetter: last letter" "M_RD_" (rmNextLetter "M_RDE" 'E'))

  , TestCase (assertEqual "goodPlaceAt" [1, 3] (goodPlaceAt "oXoXo" ".X.X."))
  , TestCase (assertEqual "goodPlaceAt" [0] (goodPlaceAt "Xoooo" "X...."))
  , TestCase (assertEqual "goodPlaceAt" [1] (goodPlaceAt "oXooo" ".X..."))
  , TestCase (assertEqual "goodPlaceAt" [1] (goodPlaceAt "oXoXo" ".XX.."))

  , TestCase (assertEqual "badPlaceAt 1" [0] (badPlaceAt "Xoooo" ".XX.."))
  , TestCase (assertEqual "badPlaceAt 2" [] (badPlaceAt "Xoooo" "X...."))
  , TestCase (assertEqual "badPlaceAt 3" [1] (badPlaceAt "oXooo" "..X.."))
  , TestCase (assertEqual "badPlaceAt 4" [1] (badPlaceAt "oXXoo" "X...."))
  , TestCase (assertEqual "badPlaceAt EFFET" [0,4] (badPlaceAt "TOLEE" "EFF_T"))

  , TestCase (assertEqual "guess: nothing good" (parseAnswer "X0 X0 X0 X0 X0") (guess "XXXXX" "OUAIS"))
  , TestCase (assertEqual "guess: 1 good letter" (parseAnswer "X0 U2 X0 X0 X0") (guess "XUXXX" "OUAIS"))
  , TestCase (assertEqual "guess: 1 bad place (n1)" (parseAnswer "I1 X0 X0 X0 X0") (guess "IXXXX" "OUAIS"))
  , TestCase (assertEqual "guess: 1 bad place (n2)" (parseAnswer "X0 X0 U1 X0 X0") (guess "XXUXX" "OUAIS"))
  , TestCase (assertEqual "guess: 1good, 1bad place" (parseAnswer "O2 A1 X0 X0 X0") (guess "OAXXX" "OUAIS"))
  , TestCase (assertEqual "guess: good letter but bad place at first" (parseAnswer "S0 A2 L0 E2 S2") (guess "SALES" "CAGES"))
  , TestCase (assertEqual "guess: bug EFFET/TOLEE" (parseAnswer "T1 O0 L0 E2 E1") (guess "TOLEE" "EFFET"))

  , TestCase (assertEqual "filterword: good places" [LetterGuess 'A' GoodPlace, LetterGuess 'B' BadPlace, LetterGuess 'C' BadLetter] (parseAnswer "A2 B1 C0") )


  , TestCase (assertEqual "checkGoodPlace" "_LORS" (checkGoodPlace (parseAnswer "A2 X0 X0 X0 X0") "ALORS" 0))

  , TestCase (assertEqual "checkBadPlace" "_LORS" (checkBadPlace (parseAnswer "X0 A1 X0 X0 X0") "ALORS" 0))
  ]


main :: IO ()
main = do
    result <- runTestTT tests
    putStrLn $ if failures result > 0 then "failed" else "success"
    putStrLn (replicate (tried result - (errors result + failures result)) '🟩')
    putStrLn (replicate (failures result) '🔴')
    putStrLn (replicate (errors result) '❌')
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

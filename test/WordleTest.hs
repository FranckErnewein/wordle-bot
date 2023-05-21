module Main where

import Test.HUnit
import Wordle
import qualified System.Exit as Exit

hasWonTest :: Test
hasWonTest = TestCase (assertEqual "should won" True (hasWon [[]]))

tests :: Test
tests = TestList [ 
  TestLabel "hasWon" hasWonTest
  ]


main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

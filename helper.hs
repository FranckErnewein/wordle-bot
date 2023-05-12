import Wordle

main = do
  fileContent <- readWordsFile
  let allWords = fiveLettersWords fileContent
  putStrLn "suggestion:"
  let s0 = idealWord allWords []
  putStrLn s0 
  putStrLn "tell me the result (⬛=0, 🟨=1 🟩=2)"
  a0 <- getLine
  let t0 =  parseTry s0 a0
  let s1 = idealWord allWords [t0]
  putStrLn s1 

  putStrLn "tell me the result (⬛=0, 🟨=1 🟩=2)"
  a1 <- getLine
  let t1 =  parseTry s0 a0
  let s2 = idealWord allWords [t0, t1]
  putStrLn s2 
  
  putStrLn "tell me the result (⬛=0, 🟨=1 🟩=2)"
  a2 <- getLine
  let t2 =  parseTry s2 a2
  let s3 = idealWord allWords [t0, t1, t2]
  putStrLn s3 

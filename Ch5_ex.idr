import System

readNumber : IO (Maybe Nat)
readNumber = do
      input <- getLine
      if all isDigit (unpack input)
        then pure (Just (cast input))
        else pure Nothing

checkGuess : Nat -> Nat -> (Bool, String)
checkGuess n x =
  if x < n then (False, "Your guess is too low") else
  if x > n then (False, "Your guess is too high") else
  (True, "=")


guess : (target : Nat) -> Nat -> IO ()
guess n guesses = do
  let nums = "\n# of guesses: " ++ (show guesses) ++ "\n"
  putStrLn (nums ++ "Guess a number between 1 and 100:")
  num <- readNumber
  case map (checkGuess n) num of
    Nothing             => do
          putStrLn "Error, not a number"
          guess n guesses
    (Just (False, str)) => do
          putStrLn str
          guess n (guesses + 1)
    (Just (True, _))    => do
          putStrLn "Correct! Congratulations :-)"


main : IO ()
main =  do  i <- time
            guess (fromIntegerNat (mod (i + 1) 100)) 0

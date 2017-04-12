printLength : IO ()
printLength = getLine >>= \input => let len = length input in
  putStrLn (show len)

printLonger : IO ()
printLonger = do  putStrLn "Write a string:"
                  first <- getLine
                  putStrLn "Write another string:"
                  second <- getLine
                  let longest = if (length first) >= (length second) then first else second
                  putStrLn ("Longest string: " ++ longest)

printLongerM : IO ()
printLongerM = (putStrLn "Write a string:") >>= \_ =>
                  getLine >>= \first =>
                  putStrLn "Write another string:" >>= \_ =>
                  getLine >>= \second =>
                  let longest = if (length first) >= (length second) then first else second in
                  putStrLn ("Longest string: " ++ longest)

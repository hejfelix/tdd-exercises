import Data.Vect

readVect : IO (n ** Vect n String)
readVect = do
  x <- getLine
  if (x == "") then
    pure (_ ** [])
  else do
    (_ ** xs) <- readVect
    pure (_ ** x :: xs)


zipInputs : IO ()
zipInputs = do
  putStrLn "Enter first vector (blank line to end)"
  (n1 ** vec1) <- readVect
  putStrLn "Enter second vector (blank line to end)"
  (n2 ** vec2) <- readVect
  case exactLength n1 vec2 of
    Nothing     => putStrLn "Vctors are of different lengths"
    Just vec2'  => printLn (zip vec1 vec2')

readToBlank : IO (List String)
readToBlank = do
  x <- getLine
  if (x == "") then
    pure []
  else do
    xs <- readToBlank
    pure (x :: xs)

readAndSave : IO ()
readAndSave = do
  xs              <- readToBlank
  fileName        <- getLine
  writeResult     <- writeFile fileName (show xs)
  case writeResult of
    Right _           => putStrLn "Successfully wrote list to file"
    Left fileError    => printLn fileError

groom : String -> String
groom s = pack (filter (\c => not (isInfixOf (cast c) "[\"]" ))(unpack s))

parseVect : List String -> (n ** Vect n String)
parseVect []        = (0 ** [])
parseVect (x :: xs) =
  case parseVect xs of (_ ** rest) => (_ ** x :: rest)

readVectFromFile : File -> IO (n ** Vect n String)
readVectFromFile file = do
  line <- fGetLine file
  case line of
    Right lineContent => pure (parseVect (split (== ',') (groom lineContent)))
    Left err          => do
      printLn err
      pure (_ ** [])

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
  maybeFileHandle <- openFile filename Read
  case maybeFileHandle of
    Right file  => readVectFromFile file
    Left err    => do
      printLn err
      pure (_ ** [])

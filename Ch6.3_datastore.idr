module Main

import Data.Vect


infixr 5 .+.

data Schema =
    SString
  | SInt
  | SChar
  | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString    = String
SchemaType SInt       = Int
SchemaType SChar      = Char
SchemaType (x .+. y)  = (SchemaType x, SchemaType y)

record DataStore where
          constructor MkData
          schema  : Schema
          size    : Nat
          items   : Vect size (SchemaType schema)

data Command : Schema -> Type where
  SetSchema : (newschema : Schema) -> Command schema
  Add       : SchemaType schema -> Command schema
  Get       : Maybe Integer -> Command schema
  Quit      : Command schema

addToStore : (items: DataStore) -> SchemaType (schema items) -> DataStore
addToStore (MkData schema size items) newItem = MkData schema _ (addToData items)
  where
      addToData : Vect oldSize (SchemaType schema) -> Vect (oldSize + 1) (SchemaType schema)
      addToData [] =  [newItem]
      addToData (x :: xs) = x :: addToData xs

display : SchemaType schema -> String
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = SChar} item = show item
display {schema = (y .+. z)} (iteml, itemr) = display iteml ++ ", " ++
                                              display itemr

getEntry : (pos : Integer) -> (dstore : DataStore) -> Maybe (String, DataStore)
getEntry pos dstore =
  let storeItems = items dstore
    in case integerToFin pos (size dstore) of
             Nothing => Just ("Out of range\n", dstore)
             (Just id) => Just (display (index id storeItems) ++ "\n", dstore)


parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) = case span (/= '"') xs of
          (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
          _                     => Nothing
    getQuoted _ = Nothing
parsePrefix SInt item = case span isDigit item of
    ("", rest)  => Nothing
    (num, rest) => Just (cast num, ltrim rest)
parsePrefix SChar str               = case unpack str of
    head :: rest => Just (head, ltrim (pack rest))
    []           => Nothing
parsePrefix (left .+. right) input = do
  (leftResult, input')    <- parsePrefix left input
  (rightResult, input'')  <- parsePrefix right input'
  Just ((leftResult,rightResult), input'')


parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input =
  do
    (res, "") <- parsePrefix schema input
    Just res

parseSchema : List String -> Maybe Schema
parseSchema ("Char" :: xs)
    = case xs of
           [] => Just SChar
           _  => do   xs_sch <- parseSchema xs
                      Just (SChar .+. xs_sch)
parseSchema ("String" :: xs)
    = case xs of
           [] => Just SString
           _ => do    xs_sch <- parseSchema xs
                      Just (SString .+. xs_sch)
parseSchema ("Int" :: xs)
    = case xs of
           [] => Just SInt
           _ => do    xs_sch <- parseSchema xs
                      Just (SInt .+. xs_sch)
parseSchema _ = Nothing

parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "add" rest      = do  restok <- parseBySchema schema rest
                                          Just (Add restok)
parseCommand schema "quit" _        = Just Quit
parseCommand schema "get" ""        = Just (Get Nothing)
parseCommand schema "get" val       = case all isDigit (unpack val) of
                                        False => Nothing
                                        True => Just (Get (Just (cast val)))
parseCommand schema "schema" rest   = do  schemaok <- parseSchema (words rest)
                                          Just (SetSchema schemaok)
parseCommand _ _ _                  = Nothing

parse : (schema: Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
    (cmd, args) => parseCommand schema cmd (ltrim args)


setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
    Z => Just (MkData schema _ [])
    S k => Nothing

showAll : Nat -> Vect size (SchemaType schema) -> String
showAll idx [] = ""
showAll idx (x :: xs) = show idx ++ ": " ++ display x ++ "\n" ++
                        showAll (idx + 1) xs

total processInput : DataStore -> String -> Maybe (String, DataStore)
processInput dstore input = case parse (schema dstore) input of
    Nothing                   => Just ("Invalid command\n", dstore)
    Just (Add item)           =>
       Just ("Added item: " ++ show (size dstore) ++ "\n", addToStore dstore item)
    Just (Get (Just pos))     => getEntry pos dstore
    Just (Get Nothing)        => Just (showAll 0 (items dstore) ++ "\n", dstore)
    Just (SetSchema schema')  => case setSchema dstore schema' of
      Nothing     => Just ("Can't update schema\n", dstore)
      Just dstore' => Just ("OK\n", dstore')
    (Just Quit)               => Nothing

main : IO ()
main = replWith (MkData (SString .+. SString .+. SInt) _ []) "Command: " processInput

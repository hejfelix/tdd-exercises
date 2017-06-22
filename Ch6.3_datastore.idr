module Main

import Data.Vect

-- %default total

infixr 5 .+.

data Schema =
    SString
  | SInt
  | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString    = String
SchemaType SInt       = Int
SchemaType (x .+. y)  = (SchemaType x, SchemaType y)

record DataStore where
          constructor MkData
          schema  : Schema
          size    : Nat
          items   : Vect size (SchemaType schema)

data Command : Schema -> Type where
  SetSchema : (newschema : Schema) -> Command schema
  Add       : SchemaType schema -> Command schema
  Get       : Integer -> Command schema
  Quit      : Command schema

{-



size : DataStore -> Nat
size (MkData size' _) = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData _ items') = items'

-}

addToStore : (items: DataStore) -> SchemaType (schema items) -> DataStore
addToStore (MkData schema size items) newItem = MkData schema _ (addToData items)
  where
      addToData : Vect oldSize (SchemaType schema) -> Vect (oldSize + 1) (SchemaType schema)
      addToData [] =  [newItem]
      addToData (x :: xs) = x :: addToData xs

display : SchemaType schema -> String
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = (x .+. y)} (iteml, itemr)
    = display iteml ++ ", " ++ display itemr

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
parsePrefix (left .+. right) input = case parsePrefix left input of
    Nothing => Nothing
    Just (leftResult, input') =>
      case parsePrefix right input' of
        Nothing => Nothing
        Just (rightResult, input'') => Just ((leftResult, rightResult), input'')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
    Just (res, "") => Just res
    Just _         => Nothing
    Nothing        => Nothing

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs)
    = case xs of
           [] => Just SString
           _ => case parseSchema xs of
                     Nothing => Nothing
                     Just xs_sch => Just (SString .+. xs_sch)
parseSchema ("Int" :: xs)
    = case xs of
           [] => Just SInt
           _ => case parseSchema xs of
                     Nothing => Nothing
                     Just xs_sch => Just (SInt .+. xs_sch)
parseSchema _ = Nothing

parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "add" rest      = case parseBySchema schema rest of
                                        Nothing => Nothing
                                        Just restok => Just (Add restok)
parseCommand schema "quit" _        = Just Quit
parseCommand schema "get" val       = case all isDigit (unpack val) of
                                        False => Nothing
                                        True => Just (Get (cast val))
parseCommand schema "schema" rest   = case parseSchema (words rest) of
                                        Nothing => Nothing
                                        Just schemaok => Just (SetSchema schemaok)
parseCommand _ _ _                  = Nothing

parse : (schema: Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
    (cmd, args) => parseCommand schema cmd (ltrim args)


setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
    Z => Just (MkData schema _ [])
    S k => Nothing


total processInput : DataStore -> String -> Maybe (String, DataStore)
processInput dstore input = case parse (schema dstore) input of
    Nothing                   => Just ("Invalid command\n", dstore)
    Just (Add item)           =>
       Just ("Added item: " ++ show (size dstore) ++ "\n", addToStore dstore item)
    Just (Get pos)            => getEntry pos dstore
    Just (SetSchema schema')  => case setSchema dstore schema' of
      Nothing     => Just ("Can't update schema\n", dstore)
      Just dstore' => Just ("OK\n", dstore')
    (Just Quit)               => Nothing

main : IO ()
main = replWith (MkData (SString .+. SString .+. SInt) _ []) "Command: " processInput
{-








searchString : Nat -> (items : Vect n String) -> (str : String) -> String
searchString idx [] str = ""
searchString idx (x :: xs) str
   = let rest = searchString (idx + 1) xs str in
     if isInfixOf str x
        then show idx ++ ": " ++ x ++ "\n" ++ rest
        else rest




-}

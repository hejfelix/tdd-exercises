module Main

import Data.Vect

infixr 5 .+.

data Schema =
    SString
  | SInt
  | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString    = String
SchemaType SInt       = Int
SchemaType (x .+. y)  = (SchemaType x, SchemaType y)

data DataStore : Type where
  MkData : (schema : Schema) ->
           (size : Nat) ->
           (items : Vect size (SchemaType schema)) ->
           DataStore

{-
data DataStore : Type
  where
    MkData : (size : Nat) ->
             (items : Vect size String) ->
             DataStore

%name DataStore dstore, dstore1

data Command =
    Add String
  | Get Integer
  | Search String
  | Size
  | Quit

size : DataStore -> Nat
size (MkData size' _) = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData _ items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items)
  where
      addToData : Vect old String -> Vect (old + 1) String
      addToData [] =  [newItem]
      addToData (x :: xs) = x :: addToData xs




parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str    = Just (Add str)
parseCommand "search" str = Just (Search str)
parseCommand "quit" _     = Just Quit
parseCommand "size" _     = Just Size
parseCommand "get" val    = case all isDigit (unpack val) of
  False => Nothing
  True => Just (Get (cast val))
parseCommand _ _        = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
  (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (dstore : DataStore) -> Maybe (String, DataStore)
getEntry pos dstore =
  let storeItems = items dstore
    in case integerToFin pos (size dstore) of
             Nothing => Just ("Out of range\n", dstore)
             (Just id) => Just (index id storeItems ++ "\n", dstore)

searchString : Nat -> (items : Vect n String) -> (str : String) -> String
searchString idx [] str = ""
searchString idx (x :: xs) str
   = let rest = searchString (idx + 1) xs str in
     if isInfixOf str x
        then show idx ++ ": " ++ x ++ "\n" ++ rest
        else rest

total processInput : DataStore -> String -> Maybe (String, DataStore)
processInput dstore input = case parse input of
     Nothing              => Just ("Invalid command\n", dstore)
     (Just (Search str))  => Just(searchString 0 (items dstore) str, dstore)
     (Just (Size))        => Just (show (size dstore) ++ " \n", dstore)
     (Just (Add item))    =>
        Just ("Added item: " ++ show (size dstore) ++ "\n", addToStore dstore item)
     (Just (Get pos))     => getEntry pos dstore
     (Just Quit)          => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
-}

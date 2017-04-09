module Main

import Data.Vect

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


searchFor : (dstore : DataStore) -> (str : String) -> DataStore -> String
searchFor dstore str (MkData _ items) = show (filter (isInfixOf str) items)

total processInput : DataStore -> String -> Maybe (String, DataStore)
processInput dstore input = case parse input of
     Nothing              => Just ("Invalid command\n", dstore)
     (Just (Search str))  => Just(searchFor dstore str dstore ++ "\n", dstore)
     (Just (Size))        => Just (show (size dstore) ++ " \n", dstore)
     (Just (Add item))    =>
        Just ("Added item: " ++ show (size dstore) ++ "\n", addToStore dstore item)
     (Just (Get pos))     => getEntry pos dstore
     (Just Quit)          => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput

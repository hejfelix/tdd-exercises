module Ch3

import Data.Vect

total describeList : List Int -> String
describeList [] = "Empty"
describeList (_ :: xs) = "Non-empty, tail = " ++ show xs

total myLength : List a -> Nat
myLength [] = 0
myLength (_ :: xs) = 1 + myLength xs

total myReverse : List a -> List a
myReverse xs = rev xs []
  where
    rev : List a -> List a -> List a
    rev [] acc = acc
    rev (x :: xs) acc = rev xs (x :: acc)

total myMap : (a -> b) -> List a -> List b
myMap f [] = []
myMap f (x :: xs) = f x :: map f xs

total myVMap : (a -> b) -> Vect n a -> Vect n b
myVMap f [] = []
myVMap f (x :: xs) = f x :: myVMap f xs

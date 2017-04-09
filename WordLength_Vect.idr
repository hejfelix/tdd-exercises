module WordLength_Vect

import Data.Vect

total allLengths : Vect len String -> Vect len Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words

total insert : Ord elem => (x : elem) -> (xsSorted : Vect len elem) -> Vect (S len) elem
insert x [] = [x]
insert x (y :: xs) = case x < y of
                          False => y :: insert x xs
                          True => x :: y :: xs


total insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) =
  let sortedTail = insSort xs in
    insert x sortedTail

totalLen : List String -> Nat
totalLen xs = foldr (\str, len => length str + len) 0 xs

data Tree elem =
    Empty
  | Node (Tree elem) elem (Tree elem)

Foldable Tree where
  foldr func init Empty         = init
  foldr func init (Node x y z)  =
      let leftFold  = foldr func init x
          rightFold = foldr func leftFold z in
          func y rightFold

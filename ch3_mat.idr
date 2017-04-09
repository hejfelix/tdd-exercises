module Ch3_Mat

import Data.Vect

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = replicate _ []
transposeMat (x :: xs) =
  let xsTrans = transposeMat xs
    in
      zipWith (::) x xsTrans

addVector : Num a => Vect n a -> Vect n a -> Vect n a
addVector xs ys = zipWith (+) xs ys

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix xs ys = zipWith addVector xs ys

dot : Num a => Vect n a -> Vect n a -> a
dot xs ys = sum (zipWith (*) xs ys)

matrixVectorMul : Num a => Vect p (Vect m a) -> Vect m a -> Vect p a
matrixVectorMul yst xs = map (dot xs) yst

mulMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
mulMatrix xs ys =
  let yst = transposeMat ys
    in map (matrixVectorMul yst) xs

module Ch4

data Shape =
    Triangle Double Double
  | Rectangle Double Double
  | Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Expr =
    Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr

evaluate : Expr -> Int
evaluate (Lit x) = x
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Sub x y) = evaluate x - evaluate y
evaluate (Mul x y) = evaluate x * evaluate y

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe (Just x) (Just y) = Just (max x y)
maxMaybe Nothing y = y
maxMaybe x Nothing = x

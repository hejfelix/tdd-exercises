
data Matter = Solid | Liquid | Gas

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

data Shape = Triangle Double Double
  | Rectangle Double Double
  | Circle Double

Eq Shape where
  (==) (Triangle x z) (Triangle x' z') = x == x' && z == z'
  (==) (Rectangle x z) (Rectangle x' z') = x == x' && z == z'
  (==) (Circle x) (Circle x') = x == x'
  (==) _ _ = False

area : Shape -> Double
area (Triangle x y) = x * y / 2.0
area (Rectangle x y) = x * y
area (Circle x) = x * x * pi

Ord Shape where
  compare x y = compare (area x) (area y)

Eq Matter where
  (==) Solid Solid = True
  (==) Liquid Liquid = True
  (==) Gas Gas = True
  (==) _ _ = False

Eq elem => Eq (Tree elem) where
  (==) Empty Empty = True
  (==) (Node left e right) (Node left' e' right') =
    left == left' && e == e' && right == right'
  (==) _ _ = False

occurrences : Eq ty => (item : ty) -> (values : List ty) -> Nat
occurrences item [] = 0
occurrences item (x :: xs) = case item == x of
                              False => occurrences item xs
                              True  => 1 + occurrences item xs

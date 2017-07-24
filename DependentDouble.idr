
data Foo : Double -> Type where
  Bar : (d : Double) -> Foo d

square : (Foo d) -> Foo (d * d)
square {d} (Bar _) = Bar (d * d)

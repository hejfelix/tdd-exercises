data Vect : Nat -> Type -> Type where
  Nil  : Vect Z a
  (::) : a -> Vect k a  -> Vect (S k) a
--
-- exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
-- exactLength {m} len input = case m == len of
--                             False => Nothing
--                             True  => Just ?a

data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
    Same : (num : Nat) -> EqNat num num



checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat Z Z          = Just Refl
checkEqNat Z (S k)      = Nothing
checkEqNat (S k) Z      = Nothing
checkEqNat (S k) (S j)  = case checkEqNat k j of
                            Nothing  => Nothing
                            Just prf => Just (cong prf)

exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case checkEqNat m len of
                              Nothing         => Nothing
                              Just Refl       => Just input


total same_cons : {xs : List a} -> {ys : List a} -> xs = y -> x :: xs = x :: ys

-- for all lists xs, ys, iff x = y, xs = ys, then x :: xs = y :: ys
total same_lists : {xs : List a} -> {ys : List a} ->
                    x = y -> xs = ys -> x :: xs = y :: ys

data ThreeEq : a -> b -> c -> Type where
     AllSame : ThreeEq x x x

total allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (x + 1) (x + 1) (x + 1)

myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse (x :: xs) = let result = myReverse xs ++ [x] in
                        ?myReverse_rhs_2

import Data.Vect

-- myReverse : Vect n elem -> Vect n elem
-- myReverse [] = []
-- myReverse (x :: xs) = reverseProof (myReverse xs ++ [x])
--   where
--     reverseProof : Vect (k + 1) elem -> Vect (S k) elem
--     reverseProof {k} result = rewrite plusCommutative 1 k in result


append_nil : Vect m elem -> Vect (plus m 0) elem
append_nil {m} xs = rewrite plusZeroRightNeutral m in xs

append_xs : Vect (S (m + k)) elem -> Vect (plus m (S k)) elem
append_xs {m} {k} xs = rewrite sym (plusSuccRightSucc m k) in xs


append : Vect n elem -> Vect m elem -> Vect (m + n) elem
append [] ys        = append_nil ys
append (x :: xs) ys = append_xs (x :: append xs ys)


reverseProof_nil :  Vect n a -> Vect (plus n 0) a
reverseProof_nil {n} xs = rewrite plusZeroRightNeutral n in xs

reverseProof_xs : Vect ((S n) + m) a -> Vect (plus n (S m)) a
reverseProof_xs {n} {m} xs = rewrite sym (plusSuccRightSucc n m) in xs


myReverse : Vect n elem -> Vect n elem
myReverse xs = reverse' [] xs
  where
    reverse' : Vect n a -> Vect m a -> Vect (n + m) a
    reverse' acc []         = reverseProof_nil acc
    reverse' acc (x :: xs)  = reverseProof_xs (reverse' (x :: acc) xs)



-- base case:
--                   0 + m = m + 0 = m
--                   0 + m = m is given by prelude definition
-- ind step:
--             (1 + n) + m = m + (1 + n)
--   <plusSuccRightSucc>   = 1 + (m + n)
--   <ind.hyp.>            = 1 + (n + m)
--   <definition of +>     = (1 + n) + m
myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m      = rewrite plusZeroRightNeutral m in Refl
myPlusCommutes (S n) m  = rewrite myPlusCommutes n m in
                            rewrite plusSuccRightSucc m n in Refl

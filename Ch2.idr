module Ch2

palindrome : Nat -> String -> Bool
palindrome n str =
  let lstr = toLower str
      guard = (length str) <= n in
    if guard then False else ((reverse lstr) == lstr)

counts : String -> (Nat, Nat)
counts str =
  let numWords = length (words str)
      numChars = length str in
        (numWords, numChars)

top_ten : Ord a => List a -> List a
top_ten xs = take 10 (reverse (sort xs))

over_length : Nat -> List String -> Nat
over_length n xs = length (filter over xs)
  where
    over = (> n) . length

evens n = [x | x<- [1..n], even x]

capitalized xs = elem (head xs) ['A'..'Z']

abbreviate xs = [head xs, last xs]

powers2 n = [2^x | x <- [1..n]]

powers2' 1 = [2]

powers2' n = (powers2' (n-1)) ++ [2^n] 
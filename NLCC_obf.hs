type A = [(B, C)]

d :: [[C]] -> C -> Int
d x y = length (map fst $ f [((j, i), n) | (m, j) <- zip x [0..], (n, i) <- zip m [0..]] y)

f :: A -> C -> A
f z w = foldl (\a b -> if length b > length a then b else a) [] 
    $ map (foldl(\c d -> if snd d /= w || d `elem` c then c else c ++ [d]) [] . g z) z

g :: A -> (B, C) -> A
g _ (_, -1) = []
g z ((j, i), n) = let p = ((j, i), n)
                      q = h z (j, i - 1)
                      r = h z (j, i + 1)
                      s = h z (j - 1, i)
                      t = h z (j + 1, i)
                      u = k z ((j, i), n)
                      v = l n
                      w = g u
                 in p : w (v q) ++ w (v r) ++ w (v s) ++ w (v t)

l :: C -> (B, C) -> (B, C)
l x ((j, i), n) = if (j >= 0 && j < 6 && i >= 0 && i < 8) && n == x then ((j, i), n) else ((-1, -1), -1)

k :: A -> (B, C) -> A
k [] _ = []
k (((j, i), n):xs) ((p, q), r) = if j /= p || i /= q then ((j, i), n) : k xs ((p, q), r) else k xs ((p, q), r)

h :: A -> B -> (B, C)
h [] _ = ((-1, -1), -1)
h (((j, i), n):xs) (p, q) = if j == p && i == q then ((j, i), n) else h xs (p, q)

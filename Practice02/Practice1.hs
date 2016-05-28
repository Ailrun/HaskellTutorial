-- Just Ignore this sentence. We do not cover this one.
module Practice1 where

---- Practice1
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = (quickSort smaller) ++ [x] ++ (quickSort notSmaller)
  where smaller = (filter (< x) xs)
        notSmaller = (filter (>= x) xs)

mergeSort :: (Ord a) => [a] -> [a]
mergeSort (x:y:z) = merge (mergeSort left) (mergeSort right)
  where l = (x:y:z)
        left = (take (div (length l) 2) l)
        right = (drop (div (length l) 2) l)
mergeSort l = l

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] rl = rl
merge ll [] = ll
merge (l:ls) (r:rs) | l < r = l : merge (ls) (r:rs)
                    | otherwise = r : merge (l:ls) (rs)

---- Practice2
palindromeChecker :: (Eq a) => [a] -> Bool
palindromeChecker l = l == (reverse l)


---- Practice3
isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

isIn :: (Eq a) => [a] -> [a] -> Bool
isIn _ [] = False
isIn x (y:ys) | isPrefixOf x (y:ys) = True
              | otherwise = isIn x ys

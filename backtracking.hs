import           Data.List (delete)
-- Combination sum
combinationSum :: (Num a, Ord a) => [a] -> a -> [[a]]
combinationSum lst 0 = [[]]
combinationSum lst target =
    aux lst [] 0 []
        where
            aux lst cur total result =
                case lst of
                  [] -> result
                  x : xs ->
                      if (total + x) == target
                         then aux lst (x : cur) (total + x) ((x : cur) : result)
                         else if (total + x) < target
                         then aux xs cur total (aux lst (x : cur) (total + x) result)
                         else aux xs cur total result

-- Permutations
permute [] = [[]]
permute xs = [x : ys | x <- xs, ys <- permute (delete x xs)]

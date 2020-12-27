fib' :: (Eq p, Num p) => p -> p
fib' 0 = 1
fib' 1 = 1
fib' n = fib' (n - 1) + fib' (n - 2)

fibonacci' :: (Eq n, Num n, Enum n) => n -> [n]
fibonacci' n = [fib' x | x <- [1..n]]

length' :: Num p => [a] -> p
length' [] = 0;
length' (x:xs) = 1 + length' xs

onlyUpper' :: [Char] -> [Char]
onlyUpper' str = [x | x <- str, x `elem` ['A'..'Z']]

initials' :: [[Char]] -> [Char]
initials' strings = [x | (x:_) <- strings]

maximum' :: Ord a => [a] -> a
maximum' [a] = a
maximum' (x:xs) = max x (maximum' xs)


take' :: (Eq t, Num t) => t -> [a] -> [a]
take' 1 (x:xs) = [x];
take' n all@(x:xs) = x:take' (n - 1) xs

replicate' :: (Eq t1, Num t1) => t2 -> t1 -> [t2]
replicate' m 1 = [m]
replicate' m n = m:replicate' m (n - 1)

debounce' :: (Num t, Ord t) => t -> t -> [t]
debounce' m 0 = [m]
debounce' m n
  | m > n = []
  | otherwise = m:debounce' (m + 1) n

increase' :: (Num m, Num n, Eq n) => m -> n -> [m]
increase' m 0 = []
increase' m n = m:increase' (m + 1) (n - 1)

repeat' :: (Eq t, Num t) => t -> [t]
repeat' m = replicate' m m


combine' :: t -> [b] -> [(t, b)]
combine' _ [] = []
combine' x (y:ys) = (x, y):combine' x ys

cartesianProduct' :: [t] -> [b] -> [(t, b)]
cartesianProduct' [] _ = []
cartesianProduct' (x:xs) all@(y:ys) = combine' x all ++ cartesianProduct' xs all

unique' :: Eq a => [a] -> [a]
unique' [a] = [a]
unique' (x:xs)
  | x `notElem` xs = x:unique' xs
  | otherwise = unique' xs

quicksort' :: Ord a => [a] -> [a]
quicksort' [] = [];
quicksort' (pivot:xs) = 
  let leftHand = quicksort' [x | x <- xs, x <= pivot]
      rightHand = quicksort' [x | x <- xs, x > pivot]
  in  leftHand ++ [pivot] ++ rightHand


skip' :: Num a => Int -> [a] -> [a]
skip' n all@(x:xs) 
 | n < length xs = elem : skip' (n + 1) all
 | n == length xs = [elem]
 | otherwise = []
 where elem = xs !! (n - 1)


elem' :: Eq t => t -> [t] -> Bool
elem' a [] = False
elem' a (x:xs) 
 | a == x = True
 | otherwise = elem' a xs

isAsc' :: Ord a => [a] -> Bool
isAsc' [] = True;
isAsc' [a] = True;
isAsc' (x:y:xs) 
 | x > y = False
 | otherwise = isAsc' xs

main :: IO ()
main = do
  print (isAsc' [-2, -1, 1,2,3,4,5, 6])
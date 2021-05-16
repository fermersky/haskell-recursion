import Data.List hiding (foldl')
import qualified Data.Map as Map

fib' :: (Eq p, Num p) => p -> p
fib' 0 = 1
fib' 1 = 1
fib' n = fib' (n - 1) + fib' (n - 2)

fibonacci' :: (Eq n, Num n, Enum n) => n -> [n]
fibonacci' n = [fib' x | x <- [1 .. n]]

length' :: Num p => [a] -> p
length' [] = 0
length' (x : xs) = 1 + length' xs

onlyUpper' :: [Char] -> [Char]
onlyUpper' str = [x | x <- str, x `elem` ['A' .. 'Z']]

initials' :: [[Char]] -> [Char]
initials' strings = [x | (x : _) <- strings]

maximum' :: Ord a => [a] -> a
maximum' [a] = a
maximum' (x : xs) = max x (maximum' xs)

take' :: (Eq t, Num t) => t -> [a] -> [a]
take' 1 (x : xs) = [x]
take' n all@(x : xs) = x : take' (n - 1) xs

replicate' :: (Eq t1, Num t1) => t2 -> t1 -> [t2]
replicate' m 1 = [m]
replicate' m n = m : replicate' m (n - 1)

debounce' :: (Num t, Ord t) => t -> t -> [t]
debounce' m 0 = [m]
debounce' m n
  | m > n = []
  | otherwise = m : debounce' (m + 1) n

increase' :: (Num m, Num n, Eq n) => m -> n -> [m]
increase' m 0 = []
increase' m n = m : increase' (m + 1) (n - 1)

repeat' :: (Eq t, Num t) => t -> [t]
repeat' m = replicate' m m

combine' :: t -> [b] -> [(t, b)]
combine' _ [] = []
combine' x (y : ys) = (x, y) : combine' x ys

cartesianProduct' :: [t] -> [b] -> [(t, b)]
cartesianProduct' [] _ = []
cartesianProduct' (x : xs) all@(y : ys) = combine' x all ++ cartesianProduct' xs all

unique' :: Eq a => [a] -> [a]
unique' [a] = [a]
unique' (x : xs)
  | x `notElem` xs = x : unique' xs
  | otherwise = unique' xs

quicksort' :: Ord a => [a] -> [a]
quicksort' [] = []
quicksort' (pivot : xs) =
  let leftHand = quicksort' [x | x <- xs, x <= pivot]
      rightHand = quicksort' [x | x <- xs, x > pivot]
   in leftHand ++ [pivot] ++ rightHand

skip' :: Num a => Int -> [a] -> [a]
skip' n all@(x : xs)
  | n < length xs = elem : skip' (n + 1) all
  | n == length xs = [elem]
  | otherwise = []
  where
    elem = xs !! (n - 1)

elem' :: Eq t => t -> [t] -> Bool
elem' a [] = False
elem' a (x : xs)
  | a == x = True
  | otherwise = elem' a xs

isAsc' :: Ord a => [a] -> Bool
isAsc' [] = True
isAsc' [a] = True
isAsc' (x : y : xs)
  | x > y = False
  | otherwise = isAsc' xs

applyTwice :: (t -> t) -> t -> t
applyTwice f x = f (f x)

zipWith' :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = [];
filter' f (x:xs) 
 | f x = x:filter' f xs
 | otherwise = filter' f xs

foldl' :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
foldl' _ b [] = b;
foldl' f init (x:xs) = foldl' f (f init x) xs

sum' :: [Integer] -> Integer
sum' = foldl' (+) 0


map' :: (t -> a) -> [t] -> [a]
map' _ [] = [];
map' f (x:xs) = f x : map' f xs

trim :: [Char] -> [Char]
trim xs = [x | x <- xs, x /= ' ']

intersperse' :: a -> [a] -> [a]
intersperse' _ [a] = [a]
intersperse' sign (x:xs) = [x] ++ [sign] ++ intersperse' sign xs

concat' :: [[a]] -> [a]
concat' [a] = a;
concat' (x:xs) = x ++ concat' xs

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
data Point = Point Float Float deriving (Show)

data Person phoneType = Person {firstname :: String, lastname :: String, age :: Int, phoneNumber :: phoneType} deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = 2 * 3.14 * r
surface (Rectangle (Point x1 x2) (Point y1 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1) 


personToString :: Person Int -> String 
personToString (Person {firstname = f, lastname = l, age = a}) = "Hello, I am " ++ f ++ " " ++ l ++ " and I am " ++ show a;


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show, Read, Enum, Bounded)

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String 
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = 
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist"
    Just (state, code) -> if state /= Taken
                            then Right code
                            else Left $ "Locker " ++ show lockerNumber ++ " is already taken"


main :: IO ()
main = do
  print $ lockerLookup number map
    where 
      number = 90
      map = Map.fromList   
          [(100,(Taken,"ZD39I"))  
          ,(101,(Free,"JAH3I"))  
          ,(103,(Free,"IQSA9"))  
          ,(105,(Free,"QOTSA"))  
          ,(109,(Taken,"893JJ"))  
          ,(110,(Taken,"99292"))  
          ]  
  -- print $ personToString person
  -- where person = Person {
  --   firstname = "Daniel", 
  --   lastname = "Skrypnik", 
  --   age = 21, 
  --   phoneNumber = 234234234
  -- }
  -- print $ isUpperAlphanum 40
  -- print $ (unique' . trim . sort) "the quick brown fox jumps over lazy dog"
  -- print $ intersperse' '.' . sort $ trim "the quick brown fox jumps over lazy dog"
  -- print $ concat' [[1..5], [1..8], [6..8]]
  -- print $ (abs . negate) 5
  -- print $ (ceiling . negate . tan . cos . max 50 $ 2)
  -- print $ negate $ abs $ sum [1,2]
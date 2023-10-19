doubleMe x y = x * 2 + y * 2

doubleSmallNumber x = if x <= 100 then x * 2 else x

lostNumbers = [4, 8, 16, 23, 48]

lostNumbers2 = [4, 9]

-- [head, ..tail]
-- [init.., last]
-- length list

prependList x = 5 : x

printListAtPos x y = x !! y

boomBang xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [y + 1 | y <- xs]

removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

{-
 - main = print $ doubleSmallNumber 101
 - main = print $ prependList lostNumbers
 - main = print $ printListAtPos lostNumbers 1
 - main = print $ [x*2 | x <- [1..10], x*2>=12, x `mod` 3 == 1]
 - main = print $ boomBang [7..13]
 - main = print $ [ x * y | x <- [2,5,10], y <- [8,10,11], x*y > 10]
 - main = print $ length' lostNumbers
 - main = print $ removeNonUppercase "PascalCas上的e"
 - -}

{-
 - Chapter 4
 -}

{-
 - Pattern matching
 - -}

lucky :: (Integral a) => a -> String
lucky 7 = "asd"
lucky y = "Not in luck"

-- main = print $ lucky 123

-- Pattern matching factorial
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- main = print $ factorial 6

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

-- main = print $ addVectors (1, 2) (-0.1, 0.1)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, x, _) = x

third :: (a, b, c) -> c
third (_, _, x) = x

addVectors3 :: (Num a) => (a, a, a) -> (a, a, a) -> (a, a, a) -> (a, a, a)
addVectors3 a b c = (first a + first b + first c, second a + second b + second c, third a + third b + third c)

-- main = print $ addVectors3 (1, 2, 3) (2, 3, 4) (-1, -0.5, 0.5)

myHead :: [a] -> a
myHead [] = error "Can not head an empty list"
myHead (x : _) = x

-- main = print $ myHead [2.2, 1]

tell :: (Show a) => [a] -> String
tell [] = "Nothing to say"
tell (x : []) = "Say a char:" ++ show x
tell (x : y : []) = "Say two chars:" ++ show x ++ show y
tell (x : y : _) = "Only say two chars:" ++ show x ++ show y

myLength :: (Num b) => [a] -> b
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

-- as mode: xs@(x:y:ys) matches x:y:ys
capital :: String -> String
capital "" = "Empty string"
capital [x] = "The only one letter is " ++ [x]
capital all@[x, y] = "The first letter of " ++ all ++ " is " ++ [x] ++ " and last letter is " ++ [y]
capital all@(x : y : _ : z : xs) = "The first letter of " ++ all ++ " is " ++ [x] ++ " and second letter is " ++ [y] ++ " and the fourth letter is " ++ [z]

-- Guard
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "OK"
  | bmi <= 25.0 = "Not ok"
  | otherwise = "Very bad"

myMax :: (Ord a) => a -> a -> a
myMax a b
  | a > b = a
  | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b = GT
  | a == b = EQ
  | a < b = LT

-- main = print $ 30 `myCompare` 20

-- Where
myBmiEx :: (RealFloat a) => a -> a -> String
myBmiEx weight height
  | bmi <= skinny = "Not ok 0"
  | bmi <= normal = "OK"
  | bmi <= fat = "Not ok 1"
  | otherwise = "Not ok 2"
  where
    bmi = weight / height ^ 2
    (skinny, normal, fat) = (18.85, 25.0, 30.0)

-- Let
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * r * h
      topArea = r ^ 2
   in sideArea + 2 * topArea

-- Case
myHeadCase :: [a] -> a
myHeadCase xs = case xs of
  [] -> error "Empty list"
  (x : _) -> x

{-
 - Chapter 5 Recursive
 -}

myMaximum :: (Ord a) => [a] -> a
myMaximum [] = error "calling maximum on empty list"
myMaximum [x] = x
myMaximum (x : xs)
  | x > maxTail = x
  | otherwise = maxTail
  where
    maxTail = myMaximum xs

runMyMaximumList = print $ myMaximum [1, 2, 3, 4, 5, 2]

runMyMaximumString = print $ myMaximum "123452"

myReplicate :: (Num i, Ord i) => i -> a -> [a]
myReplicate n x
  | n <= 0 = []
  | otherwise = x : myReplicate (n - 1) x

myTake :: (Num b, Ord b) => [a] -> b -> [a]
myTake [] b = []
myTake [x] b
  | b < 1 = []
  | otherwise = [x]
myTake (x : xs) b
  | b <= 0 = []
  | otherwise = x : myTake xs (b - 1)

runMyTake = print $ myTake [1, 2, 3] 2

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x : xs) =
  let left = quickSort [a | a <- xs, a <= x]
      right = quickSort [a | a <- xs, a > x]
   in left ++ [x] ++ right

runQuickSort = print $ quickSort "ivbenrvievhneqivqfo;jeoi2foi"

main :: IO ()
main = runQuickSort

doubleMe x = x + x 
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                        then x
                        else x*2
boomMangs xs = [if x < 10 then "BOOM!" else "BAMG!" | x <- xs, odd x]
productOfTwoLists l1 l2 = [x*y | x <- l1, y <- l2]
productOfTwoListsMoreThen50 l1 l2 = [x*y | x <- l1, y <- l2, x*y > 50]

length' xs = sum [1 | _ <- xs]

-- triangles
triangles  = [(a, b, c) | c <- [1..10], b <- [1..10], a <- [1..10]]

-- rights triangles
rightTriangles  = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']] 

addThree x y z = x + y + z

circumference r = 2 * pi * r  

lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"   

sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"  

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors a b = (fst a + fst b, snd a + snd b) 

head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x 

tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_:xs) = 1 + length'' xs

sum' :: (Num a) => [a] -> a  
sum' [] = 0  
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "This is an empty string"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0 

max' :: (Ord a) => a -> a -> a
max' x y 
    | x > y = x
    | otherwise = y

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b = GT
    | a < b = LT
    | otherwise = LT

-- initials :: String -> String -> String
-- initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
--     where (f:_) = firstname
--           (l:_) = lastname

-- Let 
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder rd ht = 
    let sideArea = 2 * pi * rd * ht
        topArea = pi * rd ^ 2
    in sideArea + 2 * topArea

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^2]

-- Case expressions

head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty lists!"  
                       (x:_) -> x  

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty"
                                               [x] -> "singleton"
                                               xs -> "multipe"

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what [] = "emty"
          what [x] = "singleton"
          what xs = "multiple"

-- Recursion     

maximus :: (Ord a) => [a] -> a
maximus [] = error "max of empty list"
maximus [x] = x
maximus (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximus xs

maximus' :: (Ord a) => [a] -> a
maximus' [] = error "max of empty list"
maximus' [x] = x
maximus' (x:xs) = max x (maximus' xs)

replicate' :: (Num i, Ord i) =>  i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise =  a `elem'` xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = 
    let min = quicksort' [a | a <- xs, a <= x]
        max = quicksort' [a | a <- xs, a > x]
    in min ++ [x] ++ max


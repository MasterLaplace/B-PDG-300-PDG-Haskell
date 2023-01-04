{-
-- EPITECH PROJECT, 2023
-- B-PDG-300-REN-3-1-PDGD01-guillaume.papineau
-- File description:
-- My
-}

mySucc :: Int -> Int
mySucc x = x + 1

myIsNeg :: Int -> Bool
myIsNeg x = x < 0

myAbs :: Int -> Int
myAbs x | x > 0 = x
        | otherwise = (x * (-1))

myMin :: Int -> Int -> Int
myMin x y | x < y = x
        | otherwise = y

myMax :: Int -> Int -> Int
myMax x y | x > y = x
        | otherwise = y

myTuple :: Int -> Int -> (Int, Int)
myTuple x y = (x, y)

myTruple :: a -> b -> c -> (a, b, c)
myTruple x y z = (x, y, z)

myFst :: (a, b) -> a
myFst (x, y) = x

mySnd :: (a, b) -> b
mySnd (x, y) = y

mySwap :: (a, b) -> (b, a)
mySwap (x, y) = (y, x)

myHead :: [a] -> a
myHead [] = error "empty list"
myHead (x:_) = x

myTail :: [a] -> [a]
myTail [] = error "empty list"
myTail (_:xs) = xs

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myNth :: [a] -> Int -> a
myNth [] _ = error "empty list"
myNth (x:_) 0 = x
myNth (_:xs) nth | (nth > myLength xs || nth < 0) = error "too large or negative"
                | otherwise = myNth xs (nth - 1)

myTake :: Int -> [a] -> [a]
myTake _ [] = error "empty list"
myTake 0 _ = []
myTake nth (x:xs) | (nth > myLength xs || nth < 0) = error "too large or negative"
                | otherwise = x : myTake (nth - 1) xs

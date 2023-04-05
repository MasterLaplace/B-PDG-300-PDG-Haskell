{-
-- EPITECH PROJECT, 2023
-- B-PDG-300-REN-3-1-PDGRUSH1-guillaume.papineau
-- File description:
-- My
-}

module My where

mySucc :: Int -> Int
mySucc x = x + 1

myIsNeg :: Int -> Bool
myIsNeg x = x < 0

myAbs :: Int -> Int
myAbs x | x > 0 = x
        | otherwise = x * (-1)

myMin :: Int -> Int -> Int
myMin x y | x < y = x
        | otherwise = y

myMax :: Int -> Int -> Int
myMax x y | x > y = x
        | otherwise = y

myTuple :: a -> b -> (a, b)
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
myNth (_:xs) nth | nth > myLength xs || nth < 0 = error "too large or neg"
                | otherwise = myNth xs (nth - 1)

myTake :: Int -> [a] -> [a]
myTake _ [] = error "empty list"
myTake 0 _ = []
myTake nth (x:xs) | nth > myLength xs || nth < 0 = x:xs
                | otherwise = x : myTake (nth - 1) xs

myDrop :: Int -> [a] -> [a]
myDrop n [] = error "List Empty"
myDrop 1 (_:sx) = sx
myDrop n (x:sx) | n < 0 = error "Negative Index"
                | myLength (x:sx) < n = []
                | otherwise = myDrop (n - 1) sx

myAppend :: [a] -> [a] -> [a]
myAppend [] a = a
myAppend (x:sx) a = x : myAppend sx a

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myAppend (myReverse xs) [x]

myInit :: [a] -> [a]
myInit [] = error "empty list"
myInit (x:xs) = myTake (myLength (x:xs) - 1) (x:xs)

myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

myUnzip :: [(a,b)] -> ([a], [b])
myUnzip sx = (myMap myFst sx, myMap mySnd sx)

myMap :: (a -> b) -> [a] -> [b]
myMap x [] = []
myMap x (s:sx) = x s :myMap x sx

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter x [] = []
myFilter x (y: sy) | x y = y :myFilter x sy
                   | otherwise = myFilter x sy

myUnfilter :: (a -> Bool) -> [a] -> [a]
myUnfilter lambda [] = []
myUnfilter lambda (x:xs) | not (lambda x) = x : myUnfilter lambda xs
                    | otherwise = myUnfilter lambda xs
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl lambda x [] = x
myFoldl lambda x (y:ys) = myFoldl lambda (lambda x y) ys

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ x [] = x
myFoldr lambda x (y:ys) = lambda y (myFoldr lambda x ys)

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition lambda [] = ([], [])
myPartition lambda (x:xs) = (myFilter lambda (x:xs), myUnfilter lambda (x:xs))

myQuickSort :: (a -> a -> Bool) -> [a] -> [a]
myQuickSort superieur [] = []
myQuickSort superieur (x:xs) =
    let superieurAX x2 = superieur x x2
        tupleIntermediaire@(left, right) = myPartition superieurAX xs
     in myAppend (myQuickSort superieur right) $ x:myQuickSort superieur left

myElem :: Eq a => a -> [a] -> Bool
myElem n [] = False
myElem n (x:sx) | n == x = True
                | otherwise = myElem n sx

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

safeNth :: [a] -> Int -> Maybe a
safeNth [] _ = Nothing
safeNth (x:_) 0 = Just x
safeNth (_:xs) nth | nth > length xs || nth < 0 = Nothing
                | otherwise = safeNth xs (nth - 1)

safeSucc :: Maybe Int -> Maybe Int
safeSucc Nothing = Nothing
safeSucc n = succ <$> n

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup n [] = Nothing
myLookup n ((x, y):sx) | n == x = Just y
                       | otherwise = myLookup n sx

maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo n Nothing _ = Nothing
maybeDo n _ Nothing = Nothing
maybeDo n x y = Just n <*> x <*> y

myIsDigit :: Char -> Bool
myIsDigit n = myElem n "-0123456789"

isOperat :: [Char] -> Bool
isOperat = foldr (\ x -> (&&) (myElem x "+-*/%")) True

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt n = if all myIsDigit n then Just (read n :: Int) else Nothing

isNum :: [Char] -> Bool
isNum = foldr (\ x -> (&&) (myElem x "-0123456789")) True

intInt :: [Char] -> Int
intInt [] = 0
intInt (x:xs) | isNum (x:xs) = read (x:xs) :: Int
    | otherwise = 0

getLineLength :: IO Int
getLineLength = length <$> getLine

concatLines :: Int -> IO String
concatLines n | n <= 0 = return ""
              | otherwise = myAppend <$> getLine <*> concatLines (n - 1)

getInt :: IO (Maybe Int)
getInt = readInt <$> getLine

myStrCmp :: String -> String -> Bool
myStrCmp [] [] = True
myStrCmp [] _ = False
myStrCmp _ [] = False
myStrCmp (x:xs) (y:ys) | x == y = myStrCmp xs ys
                    | otherwise = False

myInList :: [String] -> String -> Bool
myInList _ [] = error "empty arg"
myInList [] _ = False
myInList (x:xs) (y:ys) | myStrCmp x (y:ys) = True
                    | otherwise = myInList xs (y:ys)

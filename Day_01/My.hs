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
myDrop _ [] = error "empty list"
myDrop 0 _ = []
myDrop nth (x:xs) | nth > myLength xs || nth < 0 = []
  | otherwise = myReverse $ myTake (myLength (x:xs) - nth) (myReverse (x:xs))

myAppend :: [a] -> [a] -> [a]
myAppend [] [] = []
myAppend (x:xs) [] = x:xs
myAppend [] (y:ys) = y:ys
myAppend (x:xs) (y:ys) = x : myAppend xs (y:ys)

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

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((x1, x2):xs) = 
    let tupleIntermediaire@(l1, l2) = myUnzip xs
     in (x1 : l1, x2 : l2)

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap lambda (x:xs) = lambda x : myMap lambda xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter lambda [] = []
myFilter lambda (x:xs) | lambda x = x : myFilter lambda xs
                    | otherwise = myFilter lambda xs

myUnfilter :: (a -> Bool) -> [a] -> [a]
myUnfilter lambda [] = []
myUnfilter lambda (x:xs) | lambda x == False = x : myUnfilter lambda xs
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
     -- superieurAX :: a -> Bool
    let superieurAX x2 = superieur x x2
        tupleIntermediaire@(left, right) = myPartition superieurAX xs
     in myAppend (myQuickSort superieur right) $ x:myQuickSort superieur left

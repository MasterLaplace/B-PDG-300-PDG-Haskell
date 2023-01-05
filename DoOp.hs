{-
-- EPITECH PROJECT, 2023
-- B-PDG-300-REN-3-1-PDGD01-guillaume.papineau
-- File description:
-- DoOp
-}

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl lambda x [] = x
myFoldl lambda x (y:ys) = myFoldl lambda (lambda x y) ys

myAppend :: [a] -> [a] -> [a]
myAppend [] [] = []
myAppend (x:xs) [] = x:xs
myAppend [] (y:ys) = y:ys
myAppend (x:xs) (y:ys) = x : myAppend xs (y:ys)

myElem :: Eq a => a -> [a] -> Bool
myElem y = myFoldl (\acc x -> (x == y) || acc) False

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
safeSucc (Just x) = Just (x + 1)

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup a [] = Nothing
myLookup a ((x1, x2):xs) | x1 == a = Just x2
        | otherwise = myLookup a xs

maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo lambda Nothing _ = Nothing
maybeDo lambda _ Nothing = Nothing
maybeDo lambda (Just x) (Just y) = Just lambda <*>Just x <*> Just y


isNum :: [Char] -> Bool
isNum = foldr (\ x -> (&&) (myElem x "-0123456789")) True

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt (x:xs) | isNum (x:xs) = Just (read (x:xs) :: Int)
    | otherwise = Nothing

getLineLength :: IO Int
getLineLength = do length <$> getLine

printAndGetLength :: String -> IO Int
printAndGetLength x = do
            putStrLn x
            return (length x)

concatLines :: Int -> IO String
concatLines 0 = return ""
concatLines n = do
    line <- getLine
    concat <- concatLines (n - 1)
    return (myAppend line concat)

getInt :: IO (Maybe Int)
getInt = do readInt <$> getLine

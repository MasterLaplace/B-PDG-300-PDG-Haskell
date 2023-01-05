{-
-- EPITECH PROJECT, 2023
-- B-PDG-300-REN-3-1-PDGD01-guillaume.papineau
-- File description:
-- DoOp
-}

import My ( myFoldl, myLength )

myElem :: Eq a => a -> [a] -> Bool
myElem y = myFoldl (\acc x -> (x == y) || acc) False

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

safeNth :: [a] -> Int -> Maybe a
safeNth [] _ = Nothing
safeNth (x:_) 0 = Just x
safeNth (_:xs) nth | nth > myLength xs || nth < 0 = Nothing
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

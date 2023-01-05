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


{-
-- EPITECH PROJECT, 2023
-- B-PDG-300-REN-3-1-PDGD01-guillaume.papineau
-- File description:
-- DoOp
-}

import System.Directory.Internal.Prelude (getArgs)
import System.Exit (exitWith, ExitCode (..), exitSuccess)

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

isOperat :: [Char] -> Bool
isOperat = foldr (\ x -> (&&) (myElem x "+-*/%")) True

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt (x:xs) | isNum (x:xs) = Just (read (x:xs) :: Int)
    | otherwise = Nothing

intInt :: [Char] -> Int
intInt [] = 0
intInt (x:xs) | isNum (x:xs) = read (x:xs) :: Int
    | otherwise = 0

getLineLength :: IO Int
getLineLength = length <$> getLine

printAndGetLength :: String -> IO Int
printAndGetLength x = putStrLn x >>
            return (length x)

printBox :: Int -> IO ()
printBox x | x <= 0 = return ()
        | x == 1 = putStrLn "++"
        | otherwise =
        let fill = "+" ++ replicate (x * 2 - 2) '-' ++ "+"
            empty = "|" ++ replicate (x * 2 - 2) ' ' ++ "|"
        in putStrLn fill >>
        sequence (replicate (x - 2) (putStrLn empty)) >>
        putStrLn fill

concatLines :: Int -> IO String
concatLines 0 = return ""
concatLines n = do
    line <- getLine
    concat <- concatLines (n - 1)
    return (myAppend line concat)

getInt :: IO (Maybe Int)
getInt = readInt <$> getLine

checkArgs :: [String] -> Int -> Bool
checkArgs [] 0 = True
checkArgs [] _ = False
checkArgs (x:xs) nb | odd nb && isNum x = checkArgs xs (nb - 1)
                    | even nb && isOperat x = checkArgs xs (nb - 1)
                    | otherwise = False

calculListD :: [String] -> IO ()
calculListD [] = return ()
calculListD (x:xs) | (x:xs)!!1 == "*" = putStrLn (show (intInt ((x:xs)!!0)
                * intInt ((x:xs)!!2)))
                | (x:xs)!!1 == "%" = putStrLn (show (intInt ((x:xs)!!0)
                `mod` intInt ((x:xs)!!2)))
                | otherwise = putStrLn "Invalid"

calculListS :: [String] -> IO ()
calculListS [] = return ()
calculListS (x:xs) | (x:xs)!!1 == "+" = putStrLn (show (intInt ((x:xs)!!0)
                + intInt ((x:xs)!!2)))
                | (x:xs)!!1 == "-" = putStrLn (show (intInt ((x:xs)!!0)
                - intInt ((x:xs)!!2)))
                | (x:xs)!!1 == "/" = putStrLn (show (intInt ((x:xs)!!0)
                `div` intInt ((x:xs)!!2)))
                | otherwise = calculListD (x:xs)

main :: IO ()
main = do 
  av <- getArgs
  if checkArgs av 3 && ((av!!1 /= "/" && av!!1 /= "%") || av!!2 /= "0")
    then calculListS av >> exitSuccess
    else exitWith (ExitFailure 84)
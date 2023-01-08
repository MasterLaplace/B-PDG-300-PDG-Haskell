{-
-- EPITECH PROJECT, 2023
-- B-PDG-300-REN-3-1-PDGRUSH1-guillaume.papineau
-- File description:
-- core
-}

import System.Directory.Internal.Prelude (getArgs, exitFailure)
import System.Exit (exitWith, ExitCode (..), exitSuccess)
import My (isNum, myInList, readInt, intInt)
import Data.List (sort)

main :: IO ()
main = do 
    av <- getArgs
    str <- getLine
    if checkArgs av && checkLines (words str)
        then loop (words str) (strToTuple av)
        else exitWith (ExitFailure 84)

checkArgs :: [String] -> Bool
checkArgs [] = True
checkArgs (x:xs) | isNum x && x /= "-" = checkArgs xs
                 | otherwise = False

checkLines :: [String] -> Bool
checkLines [] = True
checkLines (x:xs) 
    | myInList 
    ["pa", "pb", "sa", "sb", "sc", "ra", "rb", "rr", "rra", "rrb", "rrr"] x
     = checkLines xs
    | otherwise = False

loop :: [String] -> ([Int], [Int]) -> IO ()
loop [] (a, b) = checkSort (a, b)
loop (x:xs) (a, b) | x == "pa" = loop xs (opPublishA (a, b))
                   | x == "pb" = loop xs (opPublishB (a, b))
                   | x == "sa" = loop xs (opSwap a, b)
                   | x == "sb" = loop xs (a, opSwap b)
                   | x == "sc" = loop xs (opSwap a, opSwap b)
                   | otherwise = funcOp (x:xs) (a, b)

funcOp :: [String] -> ([Int], [Int]) -> IO ()
funcOp [] (a, b) = checkSort (a, b)
funcOp (x:xs) (a, b) | x == "ra" = loop xs (opRotate a, b)
                   | x == "rb" = loop xs (a, opRotate b)
                   | x == "rr" = loop xs (opRotate a, opRotate b)
                   | x == "rra" = loop xs  (opRotateRev a, b)
                   | x == "rrb" = loop xs  (a, opRotateRev b)
                   | x == "rrr" = loop xs  (opRotateRev a, opRotateRev b)
                   | otherwise = checkSort (a, b)

checkSort :: ([Int], [Int]) -> IO ()
checkSort (a, b) | a == sort a = putStrLn "OK" >> exitSuccess
                 | otherwise = putStr "KO: " >> printTlist (a, b) 
                  >> exitSuccess

printTlist :: ([Int], [Int]) -> IO ()
printTlist ([], []) = return ()
printTlist (a, b) = putStrLn ('(' : (show a ++ show b) ++ ")")

strToTuple :: [String] -> ([Int], [Int])
strToTuple [] = ([], [])
strToTuple x = (map intInt x, [])

-- Operations

opRotateRev :: [Int] -> [Int]
opRotateRev str = last str : init str

opRotate :: [Int] -> [Int]
opRotate [] = []
opRotate [_] = []
opRotate (x:y:xs) = reverse $ x:reverse (y:xs)

opSwap :: [Int] -> [Int]
opSwap [] = []
opSwap [x] = [x]
opSwap (x:y:xs) = y:x:xs

opPublishA :: ([Int], [Int]) -> ([Int], [Int])
opPublishA (a, []) = (a, [])
opPublishA (a, b:bx) = (b:a, bx)

opPublishB :: ([Int], [Int]) -> ([Int], [Int])
opPublishB ([], b) = ([], b)
opPublishB (a:ax, b) = (ax, a:b)

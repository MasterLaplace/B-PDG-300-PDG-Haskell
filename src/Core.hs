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

class FuncOp a where
    funcOp :: (String, a) -> ([Int], [Int])

instance FuncOp ([Int], [Int]) where
    funcOp ("pa", x) = opPublishA x
    funcOp ("pb", x) = opPublishB x
    funcOp ("sa", (x, y)) = (opSwap x, y)
    funcOp ("sb", (x, y)) = (x, opSwap y)
    funcOp ("sc", (x, y)) = (opSwap x, opSwap y)
    funcOp ("ra", (x, y)) = (opRotate x, y)
    funcOp ("rb", (x, y)) = (x, opRotate y)
    funcOp ("rr", (x, y)) = (opRotate x, opRotate y)
    funcOp ("rra", (x, y)) = (opRotateRev x, y)
    funcOp ("rrb", (x, y)) = (x, opRotateRev y)
    funcOp ("rrr", (x, y)) = (opRotateRev x, opRotateRev y)

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
checkSort ([], []) = return ()
checkSort (a, b) | a == sort a && null b = putStrLn "OK" >> exitSuccess
                 | otherwise = putStr "KO: " >>
                 putStrLn ('(' : (show a ++ show b) ++ ")")
                  >> exitSuccess

strToTuple :: [String] -> ([Int], [Int])
strToTuple [] = ([], [])
strToTuple x = (map intInt x, [])

-- Operations

opRotateRev :: [Int] -> [Int]
opRotateRev [] = []
opRotateRev xs = last xs:init xs

opRotate :: [Int] -> [Int]
opRotate [] = []
opRotate (x:xs) = reverse $ x:reverse xs

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

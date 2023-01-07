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

data Operator = Pa | Pb | Sa | Sb | Sc | Ra | Rb | Rr | Rra | Rrb | Rrr

class ConvertOp a where
    convertOp :: String -> a

instance ConvertOp Operator where
    convertOp "pa" = Pa
    convertOp "pb" = Pb
    convertOp "sa" = Sa 
    convertOp "sb" = Sb
    convertOp "sc" = Sc
    convertOp "ra" = Ra
    convertOp "rb" = Rb
    convertOp "rr" = Rr
    convertOp "rra" = Rra
    convertOp "rrb" = Rrb
    convertOp "rrr" = Rrr

class FuncOp a b where
    -- a = Operator | b = list of numbers
    funcOp :: a -> b -> ([Int], [Int])

instance FuncOp Operator ([Int], [Int]) where
    funcOp Pa x = opPublishA x
    funcOp Pb x = opPublishB x
    funcOp Sa (x, y) = (opSwap x, y)
    funcOp Sb (x, y) = (x, opSwap y)
    funcOp Sc (x, y) = (opSwap x, opSwap y)
    funcOp Ra (x, y) = (opRotate x, y)
    funcOp Rb (x, y) = (x, opRotate y)
    funcOp Rr (x, y) = (opRotate x, opRotate y)
    funcOp Rra (x, y) = (opRotateRev x, y)
    funcOp Rrb (x, y) = (x, opRotateRev y)
    funcOp Rrr (x, y) = (opRotateRev x, opRotateRev y)

main :: IO ()
main = do 
    av <- getArgs
    str <- getLine
    if checkArgs av && checkLines (words str)
        then loop (strToType $ words str) (strToTuple av)
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

loop :: [Operator] -> ([Int], [Int]) -> IO ()
loop [] (a, b) = checkSort (a, b)
loop (x:xs) (a, b) = loop xs $ funcOp x (a, b)

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

strToType :: [String] -> [Operator]
strToType = map convertOp

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
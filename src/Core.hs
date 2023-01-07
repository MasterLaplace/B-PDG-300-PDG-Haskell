{-
-- EPITECH PROJECT, 2023
-- B-PDG-300-REN-3-1-PDGRUSH1-guillaume.papineau
-- File description:
-- core
-}

import System.Directory.Internal.Prelude (getArgs)
import System.Exit (exitWith, ExitCode (..), exitSuccess)
import My (isNum, myInList)

main :: IO ()
main = do 
  av <- getArgs
  str <- getLine
  if checkArgs av && checkLines (words str)
      then loop (words str) av
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

loop :: [String] -> [String] -> IO ()
loop _ _ = return ()
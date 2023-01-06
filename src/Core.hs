{-
-- EPITECH PROJECT, 2023
-- B-PDG-300-REN-3-1-PDGRUSH1-guillaume.papineau
-- File description:
-- core
-}

import System.Directory.Internal.Prelude (getArgs)
import System.Exit (exitWith, ExitCode (..), exitSuccess)
import My ()

main :: IO ()
main = do 
  av <- getArgs
  if av!!1 /= ""
    then exitSuccess
    else exitWith (ExitFailure 84)
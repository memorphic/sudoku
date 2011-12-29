-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  Peter Hall 2011
-- License     :  MIT
--
-- Maintainer  :  Peter Hall
-- Stability   :  
-- Portability :  
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where


import System.Environment
import Sudoku

main :: IO ()
main = do
    args        <- getArgs
    rawData     <- readFile (args !! 0) 
    sudoku      <- readSudoku rawData
    solution    <- solveSudoku sudoku
    putStr $ showSudoku solution

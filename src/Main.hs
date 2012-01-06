-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  Peter Hall 2012
-- License     :  MIT
--
-- Maintainer  :  Peter Hall
-- Stability   :  
-- Portability :  
--
-- Run with a list of files containing sudokus, to solve
-- Optionally the first argument can be -s N where N is 1 or 2, to choose 
--  wich algorithm to choose to solve the sudoku
--
-----------------------------------------------------------------------------

module Main (
    main
) where


import System.Environment (getArgs)
import Sudoku.Common
import qualified Sudoku.Solver1 as S1
import qualified Sudoku.Solver2 as S2
import Sudoku.ReadWrite
import Data.Time.Clock.POSIX (getPOSIXTime)

main :: IO ()
main = do
    args <- getArgs
    let (impl, files, runTest) = case args of 
                        ("-s":n:rest)   -> (read n, rest, False)
                        justFiles       -> (1, justFiles, False)  
                        
        doRun a i =  logTimeOf $ catch (loadAndSolve a i)
                                       (\err -> putStrLn $ show err)
                                                         
    logTimeOf $ mapM_ (doRun impl) files


getSolver i = case i of
    1 -> S1.solveSudoku
    2 -> S2.solveSudoku
    otherwise -> error $ "I don't know that solver: " ++ show i

loadAndSolve :: Int -> String -> IO ()
loadAndSolve impl file = do putStrLn $ "Loading sudoko: " ++ file ++ ""
                            rawData <- readFile file
                            sudoku <- readSudoku rawData
                            putStrLn $ "Solving with solver " ++ show impl ++ "..."
                            solution <- (getSolver impl) sudoku
                            if checkSolved solution && checkHintsUnchanged sudoku solution 
                              then putStr $ showSudoku solution
                              else error "The solution is wrong!!!"
                           


logTimeOf :: IO a -> IO a
logTimeOf action = do t0 <- getPOSIXTime
                      r  <- action
                      t1 <- getPOSIXTime
                      putStrLn $ "(In " ++ show (t1-t0) ++ ")"
                      return r
                      
                      
                      
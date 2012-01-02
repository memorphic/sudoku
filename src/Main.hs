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


import System.Environment (getArgs)
import Sudoku
import Data.Time.Clock.POSIX (getPOSIXTime)

main :: IO ()
main = do
    args <- getArgs
    logTimeOf $ if null args
                    then error "At least one argument required (file names)"
                    else mapM_ each args
    where each a = logTimeOf $ catch (loadAndSolve a)
                                     (\err -> putStrLn $ show err)

loadAndSolve :: String -> IO ()
loadAndSolve file = do putStrLn $ "Loading sudoko: " ++ file ++ ""
                       rawData <- readFile file
                       sudoku <- readSudoku rawData
                       solution <- solveSudoku sudoku
                       putStr $ showSudoku solution


logTimeOf :: IO () -> IO ()
logTimeOf action = do t0 <- getPOSIXTime
                      action
                      t1 <- getPOSIXTime
                      putStrLn $ "(In " ++ show (t1-t0) ++ ")"
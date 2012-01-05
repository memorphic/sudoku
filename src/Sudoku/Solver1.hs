-----------------------------------------------------------------------------
--
-- Module      :  Sudoku.Solver1
-- Copyright   :  2011
-- License     :  AllRightsReserved
--
-- Maintainer  :  Peter Hall
-- Stability   :  
-- Portability :  
--
-- First attempt at algorithm for Sudokus. Basic brute force - for each empty
--  cell, try each remaining possibility until it's solved. Uses a slight 
--  optimisation which is to sort the remaining empty cells so that the cell 
--  with fewest remaining options is tried next
--
-----------------------------------------------------------------------------

module Sudoku.Solver1 (

    solveSudoku
) where


import Data.Maybe
import Data.List
import Sudoku.Common

                    
solveSudoku :: Monad m => Sudoku -> m Sudoku              
solveSudoku p = case solveNext =<< checkInputError p of
                  Just p' -> return p'
                  Nothing -> fail "This puzzle is unsolvable!"

                

solveNext :: Sudoku -> Maybe Sudoku   
solveNext p 
    | completed p                = Just p -- already solved
    | unusedNear nextBlank == [] = Nothing -- not solved but no valid symbols left to try
    | otherwise                  = find completed attempts
    where
        attempts            = catMaybes $ map (solveNext . tryValue) $ unusedNear nextBlank
        tryValue            = setCell p nextBlank
        -- the next cell should be the one with the fewest options left to try, which almost
        -- doubles the speed, since it doesn't have to try as many paths
        nextBlank           = minimumWith (length . unusedNear) blanks 
        blanks              = [(i,j) | i <- [0..8], j <- [0..8], cell p (i, j) == 0]
        unusedNear c        = [1..9] \\ neighbours c
        neighbours (x, y)   = row p y ++ col p x ++ block p (x, y)

completed :: Sudoku -> Bool
completed = (notElem 0) . concat 






-- Similar to minimumBy but items are compared after applying the ordering function
-- to each one, rather than using a function that returns an ordering for each pair 
minimumWith :: Ord n => (a -> n) -> [a] -> a
minimumWith f = minimumBy (\a b -> f a `compare` f b)


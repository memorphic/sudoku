-----------------------------------------------------------------------------
--
-- Module      :  Sudoku.Solver2
-- Copyright   :  2011
-- License     :  AllRightsReserved
--
-- Maintainer  :  Peter Hall
-- Stability   :  
-- Portability :  
--
-- Second approach. Start off with two lists; completed cells, with row, col, 
--  block number and value; and incomplete cells. The algorithm moves cells 
--  from one list to the other as it tries the remaining values
--
-----------------------------------------------------------------------------

module Sudoku.Solver2 (
    solveSudoku
) where


import Data.List
import Data.Char
import Data.Maybe
import Sudoku.Common (checkInputError)


solveSudoku :: Monad m =>  [[Int]] -> m [[Int]]
solveSudoku s = case checkInputError s >> doSolve of
                    Nothing -> fail "This puzzle is unsolvable!"
                    Just p -> return $ pointsToGrid p
    where 
        doSolve = solveNext hints remaining
        remaining = map fst $ filter ((==0).snd) $ coordsWithValues
        hints = filter ((/= 0).snd) $ coordsWithValues
        coordsWithValues = map (\c@(x,y,_) -> (c, cval x y)) allCells
        cval i j = s !! i !! j
        
        allCells = [(i,j,block i j) | i <- [0..8], j <- [0..8]]
        block i j = 3 * (i `div` 3) + (j `div` 3)
    
    
-- actual algorithm
solveNext hints [] = Just hints
solveNext hints (r@(x,y,b):rem) = case catMaybes $ map try remaining of 
                                    [] -> Nothing
                                    p:_ -> Just p
                           where try v = solveNext ((r,v):hints) rem
                                 remaining = [1..9] \\ (map snd . filter isNeighbour) hints
                                 isNeighbour ((x',y',b'),_) = x==x' || y==y' || b==b'


-- this is just called once to construct the grid at the end
pointsToGrid :: [((Int,Int,Int), Int)] -> [[Int]]
pointsToGrid ps = chunksOf 9 $ map snd $ sortBy (\((x,y,_),_) ((x',y',_),_) 
                                                    -> if x == x' 
                                                          then compare y y' 
                                                          else compare x x' ) ps
                   where chunksOf k = go
                          where
                            go t = case splitAt k t of
                                     (a,b) | null a    -> []
                                           | otherwise -> a : go b


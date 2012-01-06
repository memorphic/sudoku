-----------------------------------------------------------------------------
--
-- Module      :  Sudoku.Common
-- Copyright   :  Peter Hall 2012
-- License     :  MIT
--
-- Maintainer  :  Peter Hall
-- Stability   :  
-- Portability :  
--
-- Some general purpose functions that might be used in solving a sudoku and
--  checking that the solution is correct
--
-----------------------------------------------------------------------------

module Sudoku.Common 
    (cell
    ,row
    ,rows
    ,col
    ,cols
    ,block
    ,blocks
    ,setCell
    ,checkInputError
    ,checkSolved
    ,checkHintsUnchanged
    ,Symbol
    ,Sudoku
    ,InitialGrid
    ,Solution
) where

import Data.Maybe
import Data.List

type Symbol = Int
type Sudoku = [[Symbol]]
type InitialGrid = Sudoku
type Solution = Sudoku

-- A one-off check to make sure the grid doesn't contain mistakes before we even start.
checkInputError :: InitialGrid -> Maybe InitialGrid
checkInputError p = justIf noDuplicates p
                      where noDuplicates = (all $ all allUnique) $
                                              map ($p) [blocks, rows, cols]
                            allUnique cs = filledIn cs == (nub $ filledIn cs)
                            filledIn = filter (/=0)


checkSolved :: Solution -> Bool
checkSolved s = all correct [rows, cols, blocks]
                where correct = all ([1..9]==) . map sort . ($s)
                
-- It wouldn't be a solution if it just erased the hints and filled it in from there                
checkHintsUnchanged :: InitialGrid -> Solution -> Bool
checkHintsUnchanged a b = and $ zipWith unchanged (concat a) (concat b)
                where unchanged x y = y /= 0 && (x == 0 || x == y)


cell :: Sudoku -> (Int,Int) -> Symbol
cell s (x, y) = s !! y !! x

row :: Sudoku -> Int -> [Symbol]
row s n = s !! n

rows :: Sudoku -> [[Symbol]]
rows = id 

col :: Sudoku -> Int -> [Symbol]
col s n = map (!!n) s

cols :: Sudoku -> [[Symbol]]
cols = transpose

block :: Sudoku -> (Int,Int) -> [Symbol]
block s (x, y) = concat $ map (slice r 3) $ (slice c 3) s
        where r = x - (x `mod` 3)
              c = y - (y `mod` 3)

blocks :: Sudoku -> [[Symbol]]
blocks s = [block s (x,y) | x <- [0,3,6], y <- [0,3,6]]          

setCell :: Sudoku -> (Int,Int) -> Symbol -> Sudoku
setCell s (x, y) n = 
    replaceAt s y row'
    where replaceAt l i v
            | otherwise = take i l ++ [v] ++ drop (i+1) l
          row' = replaceAt (s !! y) x n





slice :: Int -> Int -> [a] -> [a]
slice from len = take len . drop from



justIf :: Bool -> a -> Maybe a
justIf p v = if p then Just v else Nothing
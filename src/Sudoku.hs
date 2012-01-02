-----------------------------------------------------------------------------
--
-- Module      :  Sudoku
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

module Sudoku
    (showSudoku
    ,readSudoku
    ,solveSudoku
) where

import Data.Char
import Data.Maybe
import Data.List

type Symbol = Int
type Sudoku = [[Symbol]]

                    
solveSudoku :: Monad m => Sudoku -> m Sudoku              
solveSudoku p = case solveNext =<< checkInputError of
                      Just p' -> return p'
                      Nothing -> fail "This puzzle is unsolvable!"
                where 
                      -- Not very optimal, but it's a one-off check to make sure
                      -- the grid doesn't contain mistakes before we even start.
                      -- solveNext won't add duplicates in any row, col or block,
                      -- but it can get confused if duplicates are already there.
                      checkInputError = justIf noDuplicates p
                      noDuplicates = (all $ all allUnique) $
                                          map ($p) [blocks, rows, cols]
                      allUnique cs = filledIn cs == (nub $ filledIn cs)
                      filledIn = filter (/=0)
                

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


------------------------------------------------------------------------------
--
--  Accessing and modifying cells inside the puzzle
-- 
------------------------------------------------------------------------------


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


------------------------------------------------------------------------------
--
--  Reading and displaying Sudoku grids
-- 
------------------------------------------------------------------------------


readSudoku :: Monad m => String -> m Sudoku
readSudoku s = 
    let removeBadLines  = filter ((==9) . length)
        isValidChar     = isJust . charToCell
        gridRows        = map (filter isValidChar) $ lines s   
        grid            = removeBadLines $ map (catMaybes . map charToCell) gridRows
    in  if map length grid == replicate 9 9
            then return grid 
            else fail "Incorrect puzzle dimensions"
        
 
showSudoku :: Sudoku -> String
showSudoku s = (unlines $ map (intersperse ' ' . map cellToChar) s)
            

cellToChar :: Symbol -> Char  
cellToChar c = case c of
                0 -> '*'
                n -> intToDigit n

charToCell :: Monad m => Char -> m Symbol
charToCell c = case c of 
                '*' -> return 0
                n | n `elem` ['0'..'9'] -> return $ digitToInt n
                  | otherwise           -> fail $ "Unexpected digit: " ++ [n]
                
                
                


------------------------------------------------------------------------------
--
--  general purpose utils
-- 
------------------------------------------------------------------------------


slice :: Int -> Int -> [a] -> [a]
slice from len = take len . drop from


justIf :: Bool -> a -> Maybe a
justIf p v = if p then Just v else Nothing

-- Similar to minimumBy but items are compared after applying the ordering function
-- to each one, rather than using a function that returns an ordering for each pair 
minimumWith :: Ord n => (a -> n) -> [a] -> a
minimumWith f = minimumBy (\a b -> f a `compare` f b)


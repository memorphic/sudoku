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
solveSudoku p = case solveNext p of
                    Just p' -> return p'
                    Nothing -> fail "This puzzle is unsolvable!"


solveNext :: Sudoku -> Maybe Sudoku   
solveNext p 
    | completed p  = Just p -- already solved
    | unused == []  = Nothing -- not solved but no valid symbols left to try
    | otherwise     = find completed attempts
    where
        attempts            = catMaybes $ map (solveNext . tryValue) unused
        tryValue            = setCell p nextBlank
        nextBlank           = head blanks
        blanks              = [(i,j) | i <- [0..8], j <- [0..8], cell p (i, j) == 0]
        unused              = [1..9] \\ neighbours nextBlank                               
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

col :: Sudoku -> Int -> [Symbol]
col s n = map (!!n) s

block :: Sudoku -> (Int,Int) -> [Symbol]
block s (x, y) = concat $ map (slice r 3) $ (slice c 3) s
        where r = x - (x `mod` 3)
              c = y - (y `mod` 3)

setCell :: Sudoku -> (Int,Int) -> Symbol -> Sudoku
setCell s (x, y) n = 
    replaceAt s y row'
    where replaceAt l i v
            | otherwise = take i l ++ [v] ++ drop (i+1) l
          row' = replaceAt (s !! y) x n



slice :: Int -> Int -> [a] -> [a]
slice from len = take len . drop from


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
                
                
                


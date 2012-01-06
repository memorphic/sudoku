-----------------------------------------------------------------------------
--
-- Module      :  Sudoku.ReadWrite
-- Copyright   :  Peter Hall 2012
-- License     :  MIT
--
-- Maintainer  :  Peter Hall
-- Stability   :  
-- Portability :  
--
-- Functions for reading and displaying Sudoku grids
--
-----------------------------------------------------------------------------

module Sudoku.ReadWrite (
    showSudoku
    ,readSudoku
) where


import Data.Maybe
import Data.Char
import Data.List


type Symbol = Int
type Sudoku = [[Symbol]]

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
                '.' -> return 0
                '_' -> return 0
                '0' -> return 0
                n | n `elem` ['1'..'9'] -> return $ digitToInt n
                  | otherwise           -> fail $ "Unexpected digit: " ++ [n]
                
                
                

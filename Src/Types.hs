module Src.Types (
  Position(..),
  Sudoku(..),
  Matrix,
  Direction
) where
import Data.List

type Matrix = [[String]]
type Direction = (Int,Int)

data Position = None | Position {x::Int, y::Int} deriving(Show, Read, Eq, Ord)
data Sudoku = Empty | World { matrix::Matrix, minValuePosition::Position, maxValuePosition::Position} deriving(Read, Eq)

instance Show Sudoku where
  show Empty = "Empty Sudoku"
  show (World matrix _ _) = concatMap (++ "\n") $ map (intercalate " ") matrix

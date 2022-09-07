module Src.Graph(
  solve,
) where
import Src.Types(Position(..), Matrix, Sudoku(..))
import Src.Utils(is_valid_position
            ,next_position
            ,generate_direction
            ,update_matrix
            ,to_int
            ,to_string
            ,get_sudoku_dimensions)

solve::Sudoku->Sudoku
solve Empty = Empty 
solve sudoku = 
  let (isFilledSudoku, result) = dfs sudoku
  in if isFilledSudoku then result else Empty

dfs::Sudoku->(Bool, Sudoku)
dfs Empty = (False, Empty)
dfs sudoku@(World matrix currentValuePosition maxValuePosition)
  | currentValuePosition == maxValuePosition = (True, sudoku)
  | otherwise = result
  where result = any_adapted_to_sudoku (dfs_main_step sudoku) [0..8]

dfs_main_step::Sudoku->Int->(Bool, Sudoku)
dfs_main_step sudoku@(World matrix currentValuePosition maxValuePosition) n = 
  if (is_valid_position nextPosition sudoku nextValue)
    then result
  else 
    (False, Empty)
  where direction = generate_direction n
        nextPosition = next_position currentValuePosition direction
        i = x currentValuePosition
        j = y currentValuePosition
        cifras = length $ matrix !! 0 !! 0
        nextValue = to_string ((to_int $ matrix !! i !! j) + 1) (cifras)
        matrixChanged = update_matrix matrix nextPosition nextValue
        result = dfs (World matrixChanged nextPosition maxValuePosition)

any_adapted_to_sudoku::(a->(Bool,Sudoku))->[a]->(Bool,Sudoku)
any_adapted_to_sudoku _ [] = (False, Empty)
any_adapted_to_sudoku f (x:xs) 
  | condition = (True, sudoku)
  | otherwise = any_adapted_to_sudoku f xs
  where result = f x
        condition = fst result
        sudoku = snd result

module Src.Graph(
  solve,
  solve_inverse,
  unique_solution
) where
import Src.Types(Position(..), Matrix, Sudoku(..))
import Src.Utils(is_valid_position
            ,next_position
            ,generate_direction
            ,update_matrix
            ,to_int
            ,to_string
            ,get_sudoku_dimensions)

unique_solution::Sudoku->Bool
unique_solution Empty = False
unique_solution sudoku = (solve sudoku) == (solve_inverse sudoku)

solve_inverse::Sudoku->Sudoku
solve_inverse Empty = Empty
solve_inverse sudoku = 
  let (isFilledSudoku,result) = dfs sudoku [8,7..0]
  in if isFilledSudoku then result else Empty

solve::Sudoku->Sudoku
solve Empty = Empty 
solve sudoku = 
  let (isFilledSudoku, result) = dfs sudoku [0..8]
  in if isFilledSudoku then result else Empty

dfs::Sudoku->[Int]->(Bool, Sudoku)
dfs Empty _ = (False, Empty)
dfs sudoku@(World matrix currentValuePosition maxValuePosition) order
  | currentValuePosition == maxValuePosition = (True, sudoku)
  | otherwise = result
  where result = any_adapted_to_sudoku (dfs_main_step sudoku order) order

dfs_main_step::Sudoku->[Int]->Int->(Bool, Sudoku)
dfs_main_step sudoku@(World matrix currentValuePosition maxValuePosition) order n = 
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
        result = dfs (World matrixChanged nextPosition maxValuePosition) order

any_adapted_to_sudoku::(a->(Bool,Sudoku))->[a]->(Bool,Sudoku)
any_adapted_to_sudoku _ [] = (False, Empty)
any_adapted_to_sudoku f (x:xs) 
  | condition = (True, sudoku)
  | otherwise = any_adapted_to_sudoku f xs
  where result = f x
        condition = fst result
        sudoku = snd result

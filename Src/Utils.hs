module Src.Utils(
  to_int,
  to_string,
  generate_direction,
  get_sudoku_dimensions,
  update_matrix,
  next_position,
  is_valid_position
) where
import Src.Types(Position(..), Sudoku(..), Matrix, Direction)

to_int::String->Int
to_int value
  | value == "---" || value == "--" || value == "----"= -1
  | value == "xxx" || value == "xx" || value == "xxxx" = 0
  | otherwise = read value :: Int

to_string::Int->Int->String
to_string number cifras 
  | number == -1 = replicate cifras '-'
  | number == 0  = replicate cifras 'x'
  | otherwise = resultado 
  where 
    cantDigitos = length $ show number
    resultado = (replicate (cifras-cantDigitos) '0') ++ show number

get_sudoku_dimensions::Sudoku->(Int,Int)
get_sudoku_dimensions = get_dimensions
  where get_dimensions Empty = (0,0)
        get_dimensions (World matrix _ _) = 
          let rows = length matrix
              cols = length $ matrix !! 0
          in (rows, cols)

generate_direction::Int->Direction
generate_direction x
  | x == 0 = (-1, 0)
  | x == 1 = (-1, 1)
  | x == 2 = ( 0, 1)
  | x == 3 = ( 1, 1)
  | x == 4 = ( 1, 0)
  | x == 5 = ( 1,-1)
  | x == 6 = ( 0,-1)
  | x == 7 = ( 0,-1)
  | x == 8 = (-1,-1)

next_position::Position->Direction->Position
next_position None _ = None
next_position (Position x y) (a,b) = Position (x+a) (y+b)

is_valid_position::Position->Sudoku->String->Bool
is_valid_position (Position i j) sudoku@(World matrix _ _) nextValue 
  | estaFueraLimites = False
  | celdaNoPuedeSerVisitada = False
  | celdaOcupada && valorNoCoincide= False
  | celdaNoOcupada && valorExisteEnOtroLugar = False
  | otherwise = True
  where (rows, cols) = get_sudoku_dimensions sudoku
        estaFueraLimites = i < 0 || i >= rows || j < 0 || j >= cols
        valorCelda = matrix !! i !! j
        celdaNoPuedeSerVisitada = (to_int valorCelda) == -1
        celdaOcupada = (to_int valorCelda) /= 0
        celdaNoOcupada = not celdaOcupada
        valorExisteEnOtroLugar = any (elem nextValue) matrix
        valorNoCoincide = nextValue /= valorCelda

update_matrix::Matrix->Position->String->Matrix
update_matrix matrix (Position x y) value = 
  let rowSelected = matrix!!x  
      filaModificada = (take y rowSelected) ++ [value] ++ (drop (y+1) rowSelected)
      parteInicial = take x matrix
      parteFinal = drop (x+1) matrix
  in parteInicial ++ [filaModificada] ++ parteFinal

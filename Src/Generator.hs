module Src.Generator (
  start_generation
)where
import Src.Ffi
import Src.Types(Sudoku(..),Position(..),Matrix)
import Src.Utils(delete_position, to_string, to_int, update_matrix, get_sudoku_dimensions)
import Src.Graph(solve)
import Data.List

start_generation::Int->Int->String->String->Int->Sudoku
start_generation rows cols minValueStr maxValueStr cellLength
  | diffValue > rows*cols = Empty
  | minPos == maxPos = start_generation rows cols minValueStr maxValueStr cellLength
  | result /= Empty = result
  | otherwise = start_generation rows cols minValueStr maxValueStr cellLength
  where diffValue = (to_int maxValueStr) - (to_int minValueStr)
        minPos = get_random_position rows cols
        maxPos = get_random_position rows cols
        result = generate_hidato (rows,cols) minPos maxPos minValueStr maxValueStr cellLength

get_random_position::Int->Int->Position
get_random_position rows cols = Position i j
  where i = get_random_number 0 rows
        j = get_random_number 0 cols

get_random_number::Int->Int->Int
get_random_number inf sup = c_random inf sup

generate_hidato (rows,cols) minPos@(Position x1 y1) maxPos@(Position x2 y2) minValueStr maxValueStr cellLength = if solucion == Empty then Empty else resultadoFinal
  where tablero = update_matrix (update_matrix (generar_tablero_vacio rows cols cellLength) minPos minValueStr) maxPos maxValueStr
      
        valorObstaculo = to_string (-1) cellLength
        valorLibre = to_string 0 cellLength
        minimo = to_int

        cantidadObstaculos = rows * cols - (to_int maxValueStr)
        
        tableroObstaculizado = colocar_obstaculos tablero (get_cell_positions tablero valorLibre) cantidadObstaculos valorObstaculo

        sudoku = World tableroObstaculizado minPos maxPos
        solucion = solve sudoku
        matrixSolucion = if solucion == Empty then [[]] else (matrix solucion) 

        -- matrixInicial = remplazar_casillas_no_visitadas sudoku solucion cellLength
        positions = get_cell_positions tableroObstaculizado valorLibre
        cantidadValores = get_random_number 0 (length positions)
        matrixModificada = colocar_valores tableroObstaculizado matrixSolucion positions cantidadValores
        resultadoFinal = World matrixModificada minPos maxPos

generar_tablero_vacio rows cols cellLength = [ [ [ 'x' | _ <- [1..cellLength]] | j <- [0..cols-1] ] | i <- [0..rows-1]]

remplazar_casillas_no_visitadas sudokuOriginal@(World matrixOriginal _ _) sudokuSolucion@(World matrixSolucion _ _) cellLength = 
  [ [ resultado i j | j<-[0..cols-1] ] | i <- [0..rows-1]]
  where resultado i j = 
          let casillaNoVisitada = replicate cellLength 'x'
              casillaObstaculo = replicate cellLength '-'
              casillaOriginal = matrixOriginal!!i!!j
              casillaSolucion = matrixSolucion!!i!!j
          in if casillaSolucion == casillaNoVisitada then casillaObstaculo
                else if casillaOriginal == casillaNoVisitada then casillaNoVisitada
                  else casillaOriginal
        (rows,cols) = get_sudoku_dimensions sudokuOriginal 

get_cell_positions::Matrix->String->[Position]
get_cell_positions matrix valorBuscado = positions
  where (rows,cols) = get_sudoku_dimensions $ World matrix None None
        positions = delete None (nub [ if value==valorBuscado then (Position i j) else None | i <- [0..rows-1], j<-[0..cols-1], let value = matrix!!i!!j ])

colocar_valores::Matrix->Matrix->[Position]->Int->Matrix
colocar_valores matrix _ _ 0 = matrix
colocar_valores matrix matrixResultado positions cantidad = 
  let index = get_random_number 0 (length positions)
      position@(Position i j) = positions!!index
      nuevasPosiciones = delete_position positions index
      nuevaMatrix = update_matrix matrix position (matrixResultado!!i!!j) 
      nuevaCantidad = cantidad - 1
  in colocar_valores nuevaMatrix matrixResultado nuevasPosiciones nuevaCantidad

colocar_obstaculos::Matrix->[Position]->Int->String->Matrix
colocar_obstaculos matrix _ 0 _ = matrix
colocar_obstaculos matrix positions cantidad value = 
  let 
      index = get_random_number 0 (length positions)
      position = positions!!index
      nuevasPosiciones = delete_position positions index 
      nuevaCantidad = cantidad - 1
      nuevaMatrix = update_matrix matrix position value
  in colocar_obstaculos nuevaMatrix nuevasPosiciones nuevaCantidad value

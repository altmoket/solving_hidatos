module Generador (
  comenzar_generacion
)where
import Ffi
import Tipos(Sudoku(..),Position(..))
import Utils(to_int, update_matrix, get_sudoku_dimensions)
import Grafo(solve)
-- main::IO()
-- main = do
--   let sudoku = comenzar_generacion 7 8 "001" "040"
--   putStrLn $ show sudoku
comenzar_generacion rows cols valorMinimo valorMaximo digitos
  | diferenciaValor > rows*cols = Empty
  | posMenor == posMayor = comenzar_generacion rows cols valorMinimo valorMaximo digitos
  | generacion /= Empty = generacion
  | otherwise = comenzar_generacion rows cols valorMinimo valorMaximo digitos
  where diferenciaValor = (to_int valorMaximo) - (to_int valorMinimo)
        posMenor = generar_posicion_aleatoria rows cols
        posMayor = generar_posicion_aleatoria rows cols
        generacion = generar_sudoku (rows,cols) posMenor posMayor valorMinimo valorMaximo digitos
        

generar_posicion_aleatoria::Int->Int->Position
generar_posicion_aleatoria rows cols = 
  let i = generar_numero_aleatorio 0 rows
      j = generar_numero_aleatorio 0 cols
  in Position i j

generar_numero_aleatorio::Int->Int->Int
generar_numero_aleatorio limiteInferior limiteSuperiorExcluido = c_random limiteInferior limiteSuperiorExcluido

generar_sudoku (rows,cols) posMin@(Position x1 y1) posMax@(Position x2 y2) valorMinimo valorMaximo digitos =
  let tablero = update_matrix (update_matrix (generar_tablero_vacio rows cols digitos) posMin valorMinimo) posMax valorMaximo
      sudoku = World tablero posMin posMax
      solucion = solve sudoku
      resultadoFinal = World (remplazar_casillas_no_visitadas sudoku solucion digitos) posMin posMax
  in 
    if solucion == Empty then Empty else resultadoFinal

generar_tablero_vacio rows cols digitos = [ [ [ 'x' | _ <- [1..digitos]] | j <- [0..cols-1] ] | i <- [0..rows-1]]

remplazar_casillas_no_visitadas sudokuOriginal@(World matrixOriginal _ _) sudokuSolucion@(World matrixSolucion _ _) digitos = 
  let resultado i j = 
        let casillaNoVisitada = replicate digitos 'x'
            casillaObstaculo = replicate digitos '-'
            casillaOriginal = matrixOriginal!!i!!j
            casillaSolucion = matrixSolucion!!i!!j
        in if casillaSolucion == casillaNoVisitada then casillaObstaculo
              else if casillaOriginal == casillaNoVisitada then casillaNoVisitada
                else casillaOriginal
      (rows,cols) = get_sudoku_dimensions sudokuOriginal 
  in [ [ resultado i j | j<-[0..cols-1] ] | i <- [0..rows-1]]
  

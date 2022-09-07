module Src.Generador (
  comenzar_generacion
)where
import Src.Ffi
import Src.Tipos(Sudoku(..),Position(..),Matrix)
import Src.Utils(to_string, to_int, update_matrix, get_sudoku_dimensions)
import Src.Grafo(solve)
import Data.List

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
      solucion@(World matrixSolucion _ _) = solve sudoku
      matrixInicial = remplazar_casillas_no_visitadas sudoku solucion digitos
      valor = to_string 0 digitos
      posiciones = obtener_posicion_casillas matrixInicial valor
      cantidad = generar_numero_aleatorio 0 (length posiciones)
      matrixModificada = colocar_valores matrixInicial matrixSolucion posiciones cantidad
      resultadoFinal = World matrixModificada posMin posMax
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

obtener_posicion_casillas::Matrix->String->[Position]
obtener_posicion_casillas matrix valorBuscado =
  let (rows,cols) = get_sudoku_dimensions $ World matrix None None
      posiciones = delete None (nub [ if valor==valorBuscado then (Position i j) else None | i <- [0..rows-1], j<-[0..cols-1], let valor = matrix!!i!!j ])
  in posiciones

colocar_valores::Matrix->Matrix->[Position]->Int->Matrix
colocar_valores matrix _ _ 0 = matrix
colocar_valores matrix matrixResultado posiciones cantidad = 
  let index = generar_numero_aleatorio 0 (length posiciones)
      posicion@(Position i j) = posiciones!!index
      nuevasPosiciones = eliminar_posicion posiciones index
      nuevaMatrix = update_matrix matrix posicion (matrixResultado!!i!!j) 
      nuevaCantidad = cantidad - 1
  in colocar_valores nuevaMatrix matrixResultado nuevasPosiciones nuevaCantidad

eliminar_posicion::[a]->Int->[a]
eliminar_posicion [] _ = []
eliminar_posicion lista index = (take index lista) ++ (drop (index + 1) lista)
  

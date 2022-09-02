module Interaccion (
  comenzar_ejecucion
) where
import System.Process
import FileOperations(agregar_sudoku, mostrar_sudokus, seleccionar_sudoku, eliminar_sudoku)
import Tipos(Sudoku(..))
import Grafo(solve)
import Generador(comenzar_generacion)
import Utils(to_int)

clearScreen::IO()
clearScreen = do
  system "clear"
  putStr "\ESC[2J"

data Menu = Inicio | Sudokus Bool | Seleccion Int Sudoku | Resultado Int Sudoku Sudoku | Generacion Sudoku

comenzar_ejecucion::IO()
comenzar_ejecucion = mostrar Inicio

mostrar::Menu->IO()
mostrar Inicio = do
  clearScreen
  mostrar_encabezado
  mostrar_acciones Inicio
  formular_pregunta Inicio
  opcion<-getLine
  ejecutar opcion Inicio
mostrar sudokus@(Sudokus False) = do
  clearScreen
  mostrar_encabezado
  mostrar_sudokus
  mostrar_acciones sudokus
  formular_pregunta sudokus
  opcion<-getLine
  ejecutar opcion sudokus
mostrar sudokus@(Sudokus True) = do
  clearScreen
  mostrar_encabezado
  mostrar_sudokus
  mostrar_acciones sudokus
  formular_pregunta sudokus
  opcion<-getLine
  ejecutar opcion sudokus
mostrar seleccion@(Seleccion index sudoku) = do
  clearScreen
  mostrar_encabezado
  putStrLn $ "Hidato " ++ show (index + 1)
  putStr $ show sudoku
  mostrar_acciones seleccion
  formular_pregunta seleccion
  opcion <- getLine
  ejecutar opcion seleccion
mostrar result@(Resultado _ sudokuOriginal resultado) = do
  clearScreen
  mostrar_encabezado
  putStrLn $ "Hidato original"
  putStr $ show sudokuOriginal
  putStrLn $ "Hidato resuelto"
  putStr $ show resultado
  mostrar_acciones result
  formular_pregunta result
  opcion<-getLine
  ejecutar opcion result
mostrar generacion@(Generacion _ ) = do
  clearScreen
  mostrar_encabezado
  putStrLn "Generando Hidato ..."
  (rows, cols, minValue, maxValue, cantDigitos) <- obtener_valores_generacion
  let sudoku = comenzar_generacion rows cols minValue maxValue cantDigitos
      resultado = solve sudoku
  putStrLn "Sudoku Generado:"
  putStrLn $ show sudoku
  putStrLn "Resultado:"
  putStr $ show resultado
  mostrar_acciones generacion
  formular_pregunta generacion
  opcion <- getLine
  ejecutar opcion (Generacion sudoku)

mostrar_encabezado::IO()
mostrar_encabezado = do
  putStrLn "+++++++++++++++++++++++++++++++++++++++++++"
  putStrLn "Gestor de Hidatos"
  putStrLn "+++++++++++++++++++++++++++++++++++++++++++"

mostrar_acciones::Menu->IO()
mostrar_acciones Inicio = do
  putStrLn "Opciones:"
  putStrLn "1 - Mostrar Hidatos"
  putStrLn "2 - Generar Hidatos"
  putStrLn "q - Salir"
mostrar_acciones (Sudokus False) = do
  putStrLn "Opciones:"
  putStrLn "1 - Seleccionar Hidato"
  putStrLn "a - Volver atras"
  putStrLn "q - Salir"
mostrar_acciones (Sudokus True) = do
  putStrLn "Opciones:"
  putStrLn "a - Volver atras"
  putStrLn "q - Salir"
mostrar_acciones (Seleccion _ _) = do
  putStrLn "Opciones:"
  putStrLn "1 - Resolver"
  putStrLn "2 - Eliminar"
  putStrLn "a - Volver atras"
  putStrLn "q - Salir"
mostrar_acciones (Resultado _ _ _) = do
  putStrLn "Opciones:"
  putStrLn "a - Volver atras"
  putStrLn "b - Volver al inicio"
  putStrLn "q - Salir"
mostrar_acciones (Generacion _ )= do
  putStrLn "Opciones:"
  putStrLn "1 - Guardar"
  putStrLn "a - Volver atras"
  putStrLn "b - Volver al inicio"
  putStrLn "q - Salir"


formular_pregunta::Menu->IO()
formular_pregunta (Sudokus True) = do
  putStrLn "Introduce el numero del sudoku de su eleccion o alguna de las opciones anteriores"

formular_pregunta _ = do
  putStrLn "Selecciona una de las opciones anteriores"

obtener_valores_generacion = do
  putStrLn "Introduce numero de filas"
  lnRows <- getLine 
  putStrLn "Introduce el numero de columnas"
  lnCols <- getLine
  putStrLn "Introduce el valor minimo"
  lnMinValue <- getLine
  putStrLn "Introduce el valor maximo"
  lnMaxValue <- getLine
  let (rows, cols) = (to_int lnRows, to_int lnCols)
      lengthMinValue = length lnMinValue
      lengthMaxValue = length lnMaxValue
      longMin = length $ show $ rows * cols
      digitos = max (max lengthMinValue lengthMaxValue) longMin
      minValue = (replicate (digitos - lengthMinValue) '0') ++ lnMinValue
      maxValue = (replicate (digitos - lengthMaxValue) '0') ++ lnMaxValue
  return (rows, cols, minValue, maxValue, digitos)

ejecutar::String->Menu->IO()
ejecutar opcion Inicio 
  | opcion == "1" = mostrar $ Sudokus False
  | opcion == "2" = mostrar $ Generacion Empty
  | opcion == "q" = return ()
  | otherwise = mostrar Inicio
ejecutar opcion sudokus@(Sudokus False)
  | opcion == "1" = mostrar (Sudokus True)
  | opcion == "a" = mostrar Inicio
  | opcion == "q" = return ()
  | otherwise = mostrar sudokus
ejecutar opcion sudokus@(Sudokus True)
  | opcion == "q" = return ()
  | opcion == "a" = mostrar (Sudokus False)
  | otherwise = do
    let index = read opcion - 1
    sudokuSeleccionado <- seleccionar_sudoku index
    if sudokuSeleccionado == Empty then (mostrar (Sudokus False)) else mostrar (Seleccion index sudokuSeleccionado)
ejecutar opcion seleccion@(Seleccion index sudoku)
  | opcion == "1" = do
      let resultado = solve sudoku
      mostrar $ Resultado index sudoku resultado
  | opcion == "2" = do
      eliminar_sudoku index
      mostrar $ Sudokus False
  | opcion == "a" = mostrar $ Sudokus False
  | opcion == "q" = return ()
  | otherwise = mostrar $ Sudokus False
ejecutar opcion resultado@(Resultado index sudokuOriginal _)
  | opcion == "a" = mostrar $ Seleccion index sudokuOriginal
  | opcion == "b" = mostrar $ Inicio
  | opcion == "q" = return ()
  | otherwise = mostrar resultado
ejecutar opcion generacion@(Generacion sudoku)
  | opcion == "1" = do
      agregar_sudoku sudoku
      mostrar Inicio
  | opcion == "a" = mostrar generacion
  | opcion == "b" = mostrar Inicio
  | opcion == "q" = return ()
  | otherwise = mostrar generacion


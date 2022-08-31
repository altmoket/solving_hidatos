module Interaccion (
  comenzar_ejecucion
) where
import System.Process
import FileOperations(mostrar_sudokus, seleccionar_sudoku, eliminar_sudoku)
import Tipos(Sudoku(..))
import Grafo(solve)

clearScreen::IO()
clearScreen = do
  system "clear"
  putStr "\ESC[2J"

data Menu = Inicio | Sudokus Bool | Seleccion Int Sudoku | Resultado Int Sudoku Sudoku

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
  putStrLn "3 - Resolver Hidatos"
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


formular_pregunta::Menu->IO()
formular_pregunta (Sudokus True) = do
  putStrLn "Introduce el numero del sudoku de su eleccion o alguna de las opciones anteriores"

formular_pregunta _ = do
  putStrLn "Selecciona una de las opciones anteriores"


ejecutar::String->Menu->IO()
ejecutar opcion Inicio 
  | opcion == "1" = mostrar $ Sudokus False
  | opcion == "2" = putStrLn "BBBBB"
  | opcion == "3" = putStrLn "CCCCC"
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

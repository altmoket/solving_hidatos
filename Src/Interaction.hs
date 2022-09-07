module Src.Interaction (
  start_execution
) where
import System.Process
import Src.FileOperations(add_hidato, show_hidatos, select_hidato, delete_hidato)
import Src.Types(Sudoku(..))
import Src.Graph(solve)
import Src.Generator(start_generation)
import Src.Utils(to_int)

clear_screen::IO()
clear_screen = do
  system "clear"
  putStr "\ESC[2J"

data Menu = Inicio | Consulta Bool | Seleccion Int Sudoku | Resultado Int Sudoku Sudoku | Generacion Sudoku

start_execution::IO()
start_execution= mostrar Inicio

mostrar::Menu->IO()
mostrar Inicio = do
  clear_screen
  show_header
  show_actions Inicio
  get_question Inicio
  option<-getLine
  execute option Inicio
mostrar consulta@(Consulta False) = do
  clear_screen
  show_header
  show_hidatos
  show_actions consulta
  get_question consulta
  option<-getLine
  execute option consulta
mostrar consulta@(Consulta True) = do
  clear_screen
  show_header
  show_hidatos
  show_actions consulta
  get_question consulta
  option<-getLine
  execute option consulta
mostrar seleccion@(Seleccion index sudoku) = do
  clear_screen
  show_header
  putStrLn $ "Hidato " ++ show (index + 1)
  putStr $ show sudoku
  show_actions seleccion
  get_question seleccion
  option <- getLine
  execute option seleccion
mostrar result@(Resultado _ sudokuOriginal resultado) = do
  clear_screen
  show_header
  putStrLn $ "Hidato original"
  putStr $ show sudokuOriginal
  putStrLn $ "Hidato resuelto"
  putStr $ show resultado
  show_actions result
  get_question result
  option<-getLine
  execute option result
mostrar generacion@(Generacion _ ) = do
  clear_screen
  show_header
  putStrLn "Generando Hidato ..."
  (rows, cols, minValue, maxValue, cantDigitos) <- get_values_from_input
  let sudoku = start_generation rows cols minValue maxValue cantDigitos
      resultado = solve sudoku
  putStrLn "Hidato Generado:"
  putStrLn $ show sudoku
  putStrLn "Resultado:"
  putStr $ show resultado
  show_actions generacion
  get_question generacion
  option <- getLine
  execute option (Generacion sudoku)

show_header::IO()
show_header = do
  putStrLn "+++++++++++++++++++++++++++++++++++++++++++"
  putStrLn "Gestor de Hidatos"
  putStrLn "+++++++++++++++++++++++++++++++++++++++++++"

show_actions::Menu->IO()
show_actions Inicio = do
  putStrLn "Opciones:"
  putStrLn "1 - Mostrar Hidatos"
  putStrLn "2 - Generar Hidatos"
  putStrLn "q - Salir"
show_actions (Consulta False) = do
  putStrLn "Opciones:"
  putStrLn "1 - Seleccionar Hidato"
  putStrLn "a - Volver atras"
  putStrLn "q - Salir"
show_actions (Consulta True) = do
  putStrLn "Opciones:"
  putStrLn "a - Volver atras"
  putStrLn "q - Salir"
show_actions (Seleccion _ _) = do
  putStrLn "Opciones:"
  putStrLn "1 - Resolver"
  putStrLn "2 - Eliminar"
  putStrLn "a - Volver atras"
  putStrLn "q - Salir"
show_actions (Resultado _ _ _) = do
  putStrLn "Opciones:"
  putStrLn "a - Volver atras"
  putStrLn "b - Volver al inicio"
  putStrLn "q - Salir"
show_actions (Generacion _ )= do
  putStrLn "Opciones:"
  putStrLn "1 - Guardar"
  putStrLn "a - Volver atras"
  putStrLn "b - Volver al inicio"
  putStrLn "q - Salir"


get_question::Menu->IO()
get_question (Consulta True) = do
  putStrLn "Introduce el numero del sudoku de su eleccion o alguna de las opciones anteriores"
get_question _ = do
  putStrLn "Selecciona una de las opciones anteriores"

get_values_from_input = do
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

execute::String->Menu->IO()
execute option Inicio 
  | option == "1" = mostrar $ Consulta False
  | option == "2" = mostrar $ Generacion Empty
  | option == "q" = return ()
  | otherwise = mostrar Inicio
execute option consulta@(Consulta False)
  | option == "1" = mostrar (Consulta True)
  | option == "a" = mostrar Inicio
  | option == "q" = return ()
  | otherwise = mostrar consulta
execute option consulta@(Consulta True)
  | option == "q" = return ()
  | option == "a" = mostrar (Consulta False)
  | otherwise = do
    let index = read option - 1
    sudokuSeleccionado <- select_hidato index
    if sudokuSeleccionado == Empty then (mostrar (Consulta False)) else mostrar (Seleccion index sudokuSeleccionado)
execute option seleccion@(Seleccion index sudoku)
  | option == "1" = do
      let resultado = solve sudoku
      mostrar $ Resultado index sudoku resultado
  | option == "2" = do
      delete_hidato index
      mostrar $ Consulta False
  | option == "a" = mostrar $ Consulta False
  | option == "q" = return ()
  | otherwise = mostrar $ Consulta False
execute option resultado@(Resultado index sudokuOriginal _)
  | option == "a" = mostrar $ Seleccion index sudokuOriginal
  | option == "b" = mostrar $ Inicio
  | option == "q" = return ()
  | otherwise = mostrar resultado
execute option generacion@(Generacion sudoku)
  | option == "1" = do
      add_hidato sudoku
      mostrar Inicio
  | option == "a" = mostrar generacion
  | option == "b" = mostrar Inicio
  | option == "q" = return ()
  | otherwise = mostrar generacion


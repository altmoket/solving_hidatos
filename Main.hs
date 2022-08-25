module Main where
import Ffi

import Grafo (solve)
import Tests(sudoku1, sudoku2, sudoku3)
main = do
  menu
-- main = do putStrLn $ show $ c_random 4 9

menu = do
  putStrLn "Selecciona una de las siguientes opciones"
  putStrLn "1 - Generar Hidato"
  putStrLn "2 - Resolver Hidato"
  putStrLn "3 - Ver Hidatos Disponibles"
  putStrLn "q - Salir"

  putStrLn ""
  putStrLn "Opcion: "
  a<-getLine
  if a == 3 then putStrLn "Corre" else "Salta"
-- menu 1 = do
--   putStrLn "Generando Hidato..."
-- menu 2 = do
--   putStrLn "Resolviendo Hidato..."
-- menu 3 = do
--   putStrLn "Mostrando Hidatos..."
--   putStrLn sudoku1
--   putStrLn sudoku2
--   putStrLn sudoku3
-- menu 4 = 
  


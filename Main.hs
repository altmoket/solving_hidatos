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

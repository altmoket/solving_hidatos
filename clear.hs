import System.Process
clearScreen::IO()
clearScreen = do
  system "clear"
  putStr "\ESC[2J"

menu "0" = do
  clearScreen
  menu_header 
  menu_options "0"
  option <- getLine
  menu option
menu "1" = do
  clearScreen
  menu_header
  menu_options "1"
  option <- getLine
  menu option
menu "2" = do
  clearScreen
  menu_header
  menu_options "2"
  option <- getLine
  menu option
menu "3" = do
  clearScreen
  menu_header
  menu_options "3"
  option <- getLine
  menu option
menu "q" = do
  clearScreen
  putStrLn "Hasta la proxima"
menu _ = do 
  menu "0"

menu_header = do
  putStrLn "-----------------------------------------"
  putStrLn "Hidato's Center Panel"
  putStrLn "-----------------------------------------"

menu_options "0" = do
  putStrLn "Opciones: "
  putStrLn "1- Mostrar Hidatos"
  putStrLn "2- Generar Hidato"
  putStrLn "3- Resolver Hidato"
  putStrLn "q- Salir"
  putStrLn "Selecciona una opcion:"
menu_options "1" = do
  putStrLn "Hidatos Disponibles: "
  putStrLn "Opciones: "
  putStrLn "1- Mostrar Hidatos"
  putStrLn "2- Generar Hidato"
  putStrLn "3- Resolver Hidato"
  putStrLn "q- Salir"
  putStrLn "Selecciona una opcion:"
menu_options _ = do
  putStrLn "Vete al carajo"

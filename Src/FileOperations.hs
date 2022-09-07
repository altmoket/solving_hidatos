module Src.FileOperations(
  show_hidatos,
  add_hidato,
  delete_hidato,
  select_hidato
) where
import System.IO
import System.Directory
import Data.List
import Control.Monad
import Src.Types(Sudoku(..), Position(..))
import Src.Utils(get_sudoku_dimensions)

hidatos_number::IO Int
hidatos_number = do
  contents <- readFile "hidatos.txt"
  let numero = read ((lines contents)!!0) :: Int
  return numero

all_matrix = do
  contents <- readFile "hidatos.txt" 
  let allLines = lines contents
      resultWithoutFirstLine = drop 1 allLines
      posicionesCulminacion = elemIndices "" resultWithoutFirstLine
      f i = 
        let Just index = elemIndex i posicionesCulminacion
            salto = if index == 0 then 3 else (posicionesCulminacion!!(index-1) + 1) + 3
            result = drop salto (take i resultWithoutFirstLine)
        in result
      matricesSudoku = map f posicionesCulminacion
  return matricesSudoku

all_positions = do
  contents <- readFile "hidatos.txt"
  let allLines = lines contents
      linesWithoutFirstPosition = drop 1 allLines
      emptyPositions = elemIndices "" linesWithoutFirstPosition
      f (index, realIndex)
        | index == 0 = 
          let lineOfFirstPosition = linesWithoutFirstPosition !! 1
              lineOfSecondPosition = linesWithoutFirstPosition !! 2
              [a1,b1] = words lineOfFirstPosition
              [a2,b2] = words lineOfSecondPosition
              x1 = read a1
              y1 = read b1 
              x2 = read a2
              y2 = read b2 
          in (Position x1 y1, Position x2 y2)
        | otherwise = 
          let realIndexAnterior = emptyPositions !! (index - 1)
              lineOfFirstPosition = linesWithoutFirstPosition !! (realIndexAnterior + 2)
              lineOfSecondPosition = linesWithoutFirstPosition !! (realIndexAnterior + 3)
              [a1,b1] = words lineOfFirstPosition
              [a2,b2] = words lineOfSecondPosition
              x1 = read a1
              y1 = read b1 
              x2 = read a2
              y2 = read b2 
          in (Position x1 y1, Position x2 y2)
      result = map f (zip [0..] emptyPositions)
  return result
  

show_hidatos = do
  handle <- openFile "hidatos.txt" ReadMode
  contents <- hGetContents handle
  
  putStrLn "Listado de sudokus"
  numeroDeSudokus <- hidatos_number
  matricesSudoku <- all_matrix

  putStrLn $ "Cantidad: " ++ show numeroDeSudokus

  forM (zip [1..] matricesSudoku) (\(index,matrix)->do
    let encabezado = "+++++++++++++++++++++++++++++++++++++\n"
        encabezadoBody = "Hidato " ++ show index ++ "\n"
        cuerpo = unlines matrix
        footer = "\n"
    putStr $ encabezado ++ encabezadoBody ++ cuerpo ++ footer)
  
  hClose handle

add_hidato sudoku@(World matrix minValuePos maxValuePos) = do
  contents <- readFile "hidatos.txt"
  (tempName,tempHandle) <- openTempFile "." "temp"

  -- Numero de sudokus
  numero <- hidatos_number
  let contenido = lines contents
      result = unlines $ drop 1 contenido
      sudokuAAgregar = concatMap (++ "\n") $ map (intercalate " ") matrix
      posicionMenor = show (x minValuePos) ++ " " ++ show (y minValuePos)
      posicionMayor = show (x maxValuePos) ++ " " ++ show (y maxValuePos)
      (rows, cols) = get_sudoku_dimensions sudoku
      dimensiones = show rows ++ " " ++ show cols
  hPutStrLn tempHandle (show $ numero + 1)
  hPutStr tempHandle result
  hPutStrLn tempHandle dimensiones
  hPutStrLn tempHandle posicionMenor
  hPutStrLn tempHandle posicionMayor
  hPutStrLn tempHandle sudokuAAgregar
   
  removeFile "hidatos.txt"
  renameFile tempName "hidatos.txt"
  hClose tempHandle
add_hidato _ = do return ()

delete_hidato index = do
  numero <- hidatos_number
  if index < 0 || index >= numero then return () else do
    contents <- readFile "hidatos.txt"
    (tempName,tempHandle) <- openTempFile "." "temp"

    matrices <- all_matrix
    let allLines = lines contents
        resultWithoutFirstLine = drop 1 allLines
        posicionesCulminacion = elemIndices "" resultWithoutFirstLine
        inf = if index == 0 then 0 else posicionesCulminacion!!(index-1)
        sup = if index == 0 then (posicionesCulminacion!!index)+1 else (posicionesCulminacion!!index)
        r1 = take inf resultWithoutFirstLine
        r2 = drop sup resultWithoutFirstLine
        result = r1 ++ r2

    hPutStrLn tempHandle (show $ numero - 1)
    hPutStr tempHandle (unlines result)

    removeFile "hidatos.txt"
    renameFile tempName "hidatos.txt"
    hClose tempHandle

select_hidato index = do
  numero <- hidatos_number
  if index < 0 || index >= numero then return (Empty) else do
    matrices <- all_matrix
    allPositions <- all_positions
    let matrix = map words (matrices!!index)
        positions = allPositions!!index
        minPos = fst positions
        maxPos = snd positions
    return (World matrix minPos maxPos)

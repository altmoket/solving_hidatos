module Main where
import Ffi
main = do putStrLn $ show $ c_random 1 5

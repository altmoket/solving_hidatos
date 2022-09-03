{-# LANGUAGE ForeignFunctionInterface #-}
module Src.Ffi where
import Foreign
import Foreign.C.Types
foreign import ccall "./clibrary/lib.h getRandomInteger" c_random :: Int -> Int -> Int

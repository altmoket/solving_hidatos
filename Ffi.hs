{-# LANGUAGE ForeignFunctionInterface #-}
module Ffi where
import Foreign
import Foreign.C.Types
foreign import ccall "lib.h getRandomInteger" c_random :: Int -> Int -> Int

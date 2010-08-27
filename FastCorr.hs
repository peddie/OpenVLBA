module FastCorr where

import Data.Vector.Storable as V
import Data.Array.CArray
import System.IO.Unsafe 
import Complex

import qualified Math.FFT as FFT

-- oh the humanity! (really please don't do anything but read the CArray)
vectorToCArray :: (Storable a) => Vector a -> CArray Int a
vectorToCArray v = let (ptr, offset, len) = unsafeToForeignPtr v in
                     unsafePerformIO $ unsafeForeignPtrToCArray ptr (1, len)

-- likewise abandon all hope all ye who enter here
carrayToVector :: (Storable a) => CArray Int a -> Vector a
carrayToVector ca = let (len, ptr) = toForeignPtr ca in
                      unsafeFromForeignPtr ptr 0 len

dftRC :: Vector Double -> Vector (Complex Double)
{-# INLINE dftRC #-}
dftRC = carrayToVector . FFT.dftRC . vectorToCArray 

dftCR :: Vector (Complex Double) -> Vector Double
{-# INLINE dftCR #-}
dftCR = carrayToVector . FFT.dftCR . vectorToCArray 

conv :: Vector Double -> Vector Double -> Vector Double
conv a b = let fa = dftRC a
               fb = dftRC b
            in dftCR $ V.zipWith (*) fa fb

xCorr :: Vector Double -> Vector Double -> Vector Double
xCorr a b = conv (V.reverse a) b

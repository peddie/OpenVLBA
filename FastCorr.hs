module FastCorr where

import Data.Vector.Storable as V
import Data.Array.CArray
import System.IO.Unsafe 

import Math.FFT

-- oh the humanity! (really please don't do anything but read the CArray)
vectorToCArray :: (Storable a) => Vector a -> CArray Int a
vectorToCArray v = let (ptr, offset, len) = unsafeToForeignPtr v in
                     unsafePerformIO $ unsafeForeignPtrToCArray ptr (1, len)

-- likewise abandon all hope all ye who enter here
carrayToVector :: (Storable a) => CArray Int a -> Vector a
carrayToVector ca = let (len, ptr) = toForeignPtr ca in
                      unsafeFromForeignPtr ptr 0 len

xCorr :: Vector Double -> Vector Double -> Vector Double
xCorr a b = let fa = carrayToVector $ dftRC $ vectorToCArray a
                fb = carrayToVector $ dftRC $ vectorToCArray b
            in
                carrayToVector $ dftCR $ vectorToCArray $ V.zipWith (*) fa fb


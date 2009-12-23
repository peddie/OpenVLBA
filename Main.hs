module Main where

import System.Random
import Control.Monad
import Graphics.Rendering.Chart.Simple

data Vector3 = Vector3 { x :: Double 
                       , y :: Double
                       , z :: Double 
                       } deriving (Show)

-- equality
instance Eq Vector3 where
    (Vector3 x1 y1 z1) == (Vector3 x2 y2 z2) = and [(x1 == x2), (y1 == y2), (z1 == z2)]

-- Cross product, addition
instance Num Vector3 where
    (Vector3 x1 y1 z1) * (Vector3 x2 y2 z2) = Vector3 (y1*z2) (z1*x2) (x1*y2)
    (Vector3 x1 y1 z1) + (Vector3 x2 y2 z2) = Vector3 (x1+x2) (y1+y2) (z1+z2)
    (Vector3 x1 y1 z1) - (Vector3 x2 y2 z2) = Vector3 (x1-x2) (y1-y2) (z1-z2)
    fromInteger x = Vector3 (fromInteger x) (fromInteger x) (fromInteger x)

{-
instance Fractional Vector3 where
    fromRational x = Vector3 (fromRational x) (fromRational y) (fromRational z)
-}

mag :: Vector3 -> Double
mag (Vector3 x y z) = sqrt $ sum $ map (^2) [x, y, z]

hat :: Vector3 -> Vector3
hat v@(Vector3 x y z) = Vector3 (x / m) (y / m) (z / m)
                      where m = mag v

-- Dot product
(.*) :: Vector3 -> Vector3 -> Double
(Vector3 x1 y1 z1) .* (Vector3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

-- create stream of uniformly distributed random number on [0,1)
uniformnoise :: Int -> [Double]
uniformnoise seed = randoms $ mkStdGen seed

-- Create stream of gaussian distributed random numbers applying a Box-Muller transform 
-- to an input stream of uniformly distributed random numbers 
boxmuller (x:y:rands) = sqrt (-2 * log x) * cos (2*pi*y) : sqrt (-2 * log x) * sin (2*pi*y) : boxmuller rands

-- stream of random numbers with gaussian distn. with specified variance
gaussiannoise seed variance = map ((sqrt variance) *) $ boxmuller $ uniformnoise seed

-- make various types of signals that our sources could produce

-- sine wave
makeSineSignal :: Double -> Double -> Double -> [Double]
makeSineSignal frequency timestep starttime = map sin $ map (2 * pi * frequency *) [starttime,(starttime+timestep)..]

-- gaussian noise signal
makeWhiteSignal :: Int -> Int -> Double -> Int -> [Double]
makeWhiteSignal source_seed random_seed variance starttime = 
    take starttime $ gaussiannoise random_seed variance ++ gaussiannoise source_seed variance

-- additive white gaussian noise (AWGN) channel model with variance specified applied to signal
noiseifyS seed variance signal = zipWith (+) signal $ gaussiannoise seed variance
noiseify = noiseifyS 22

-- a reciever has a Vector3 position and a samplerate 
data Receiver = Receiver Vector3 Double

-- a source has a Vector3 position, a source_seed which should be unique to the source and a variance (ie. intenity)
data Source = Source Vector3 Int Double

-- speed of light
c = 3e8

distance a b = mag $ a - b
-- time taken for light to get from a to b
lightdelay a b = (distance a b) / c

receive :: Source -> Int -> Receiver -> [Double]
receive (Source src_pos src_seed variance) random_seed (Receiver recv_pos samplerate) = 
    makeWhiteSignal src_seed random_seed variance $ round $ delay*samplerate
    where
      delay = lightdelay src_pos recv_pos
      
receivenoisyS seed variance src recv = noiseifyS seed variance $ receive src (seed+1) recv
receivenoisy = receivenoisyS 42

-- Correlation now implemented in tail recursive style, here is the old version for reference:
-- correlation 0 _ _ = 0
-- correlation length (a:as) (b:bs) = a*b + correlation (length-1) as bs

correlation' 0 _ _ acc = acc
correlation' length (a:as) (b:bs) acc = correlation' (length-1) as bs $! acc + a*b
correlation length as bs = correlation' length as bs 0

crosscorrelation length as (b:bs) = correlation length as (b:bs) : crosscorrelation length as bs

-- TEST DATA
r_a :: Receiver
r_a = Receiver (Vector3 0 0 0) 1e-6
r_b :: Receiver
r_b = Receiver (Vector3 50000 0 0) 1e-6
r_c :: Receiver
r_c = Receiver (Vector3 0 50000 0) 1e-6

s_a :: Source
s_a = Source (Vector3 0 0 50000) 1 2.0

range :: Double
range = 1000.0
plotsignal name signal = plotPDF (name ++ ".pdf") [1..range] $ take 1000 signal

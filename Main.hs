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

makeSignal :: Double -> Double -> Double -> [Double]
makeSignal frequency timestep starttime = map sin $ map (2 * pi * frequency *) [starttime,(starttime+timestep)..]

-- create stream of uniformly distributed random number on [0,1)
uniformnoise :: Int -> [Double]
uniformnoise seed = randoms (mkStdGen seed)

-- Create stream of gaussian distributed random numbers applying a Box-Muller transform 
-- to an input stream of uniformly distributed random numbers 
boxmuller (x:y:rands) = sqrt (-2 * log x) * cos (2*pi*y) : sqrt (-2 * log x) * sin (2*pi*y) : boxmuller rands

-- stream of random numbers with gaussian distn. with specified variance
gaussiannoise variance = map ((sqrt variance) *) $ boxmuller $ uniformnoise 22

-- additive white gaussian noise (AWGN) channel model with variance specified applied to signal
noiseify variance signal = zipWith (+) signal $ gaussiannoise variance

data Station = Station { pos :: Vector3
                         , timing :: Double 
                       } deriving (Show)

-- timing means frequency for a Source and sample time for a Receiver
type Receiver = Station
type Source = Station

c = 3e8

stationtiming (Station pos timing) = timing
stationpos (Station pos timing) = pos

stationdist a b = mag $ stationpos a - stationpos b
stationdelay a b = stationdist a b / c

receive :: Source -> Receiver -> [Double]
receive src@(Station srcpos freq) recv@(Station recvpos timestep) = makeSignal freq timestep delay
    where
      delay = stationdelay src recv
      
receivenoisy variance src recv = noiseify variance $ receive src recv

correlation 0 as bs = 0
correlation length (a:as) (b:bs) = a*b + correlation (length-1) as bs

crosscorrelation length as (b:bs) = correlation length as (b:bs) : crosscorrelation length as bs

-- TEST DATA
r_a :: Receiver
r_a = Station { pos = Vector3 50000 0 0, timing = 1e-6}
r_b :: Receiver
r_b = Station { pos = Vector3 0 50000 0, timing = 1e-6}
r_c :: Receiver
r_c = Station { pos = Vector3 50000 50000 0, timing = 1e-6}

s_a :: Source
s_a = Station { pos = Vector3 0 0 50000, timing = 1e3}

range :: Double
range = 1000.0
plotsignal name signal = plotPDF (name ++ ".pdf") [1..range] $ take 1000 signal

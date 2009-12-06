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

data Station = Station { pos :: Vector3
                         , timing :: Double 
                       } deriving (Show)

-- timing means frequency for a Source and sample time for a Receiver

type Receiver = Station
type Source = Station

c = 2.9979e8

receive :: Source -> Receiver -> [Double]
receive src@(Station srcpos freq) recv@(Station recvpos timestep) = makeSignal freq timestep delay
    where 
      dist = mag $ recvpos - srcpos
      delay = dist / c

randomnoise :: Int -> [Double]
randomnoise seed = randoms (mkStdGen seed)

gaussiannoise (x:y:rands) = sqrt ((-2 * log x)) * cos(2*pi*y):gaussiannoise rands

mkgaussiannoise = gaussiannoise $ randomnoise 22

noiseify signal variance = zipWith (+) signal $ map ((sqrt variance) *) mkgaussiannoise

plotsignal :: (Double t, PlotWindowType ([t] -> t1)) => t1
plotsignal signal = plotWindow [1..100] $ take 100 signal
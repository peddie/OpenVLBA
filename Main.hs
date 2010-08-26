module Main where

import System.Random
import Graphics.Gnuplot.Simple
import qualified Data.Vector.Unboxed as V

main = do
       plot2dcorr "damnit" 5000 100 rcv_a rcv_b rcv_c

-- Positions
data Vector3 = Vector3 { x :: Double 
                       , y :: Double
                       , z :: Double 
                       } deriving (Show, Eq)

vsub :: Vector3 -> Vector3 -> Vector3
vsub (Vector3 a1 a2 a3) (Vector3 b1 b2 b3) = Vector3 (a1-b1) (a2-b2) (a3-b3)

mag :: Vector3 -> Double
mag (Vector3 x y z) = sqrt $ sum $ map (^2) [x, y, z]

distance :: Vector3 -> Vector3 -> Double
distance a b = mag $ a `vsub` b

-- speed of light
c = 1 -- 3e8

-- time taken for light to get from a to b
lightdelay a b = (distance a b) / c



-- RandomizedSignals simply have a variance and a seed.  
data RandomizedSignal = RS Double Int deriving (Show, Eq)

-- SignalMeasurements have a sample time, a (globally referenced,
-- e.g. GPS) start time and a sample duration.  
data SignalMeasurement = SM Double Double Double deriving (Show, Eq)

-- a reciever has a Vector3 position and a SignalMeasurement at that
-- position
data Receiver = Receiver Vector3 SignalMeasurement deriving (Show, Eq)

-- a source has a Vector3 position, a source_seed which should be
-- unique to the source and a variance (ie. intensity)
data Source = Source Vector3 RandomizedSignal deriving (Show, Eq)

-- create stream of uniformly distributed random number on [0,1)
uniformnoise :: Int -> Int -> V.Vector Double
uniformnoise seed len = V.fromList $ take len $ randoms $ mkStdGen seed

-- Create stream of gaussian distributed random numbers applying a
-- Box-Muller transform to an input stream of uniformly distributed
-- random numbers
boxmuller :: V.Vector Double -> V.Vector Double -> V.Vector Double
boxmuller seed_a seed_b = (V.++) (fst vecs) (snd vecs)
           where bm (x,y) = ((bmc x y), (bms x y))
                 bmc x y = sqrt(-2 * log x) * cos (2 * pi * y)
                 bms x y = sqrt(-2 * log x) * sin (2 * pi * y)
                 vecs = V.unzip $ V.map bm $ V.zip seed_a seed_b

-- stream of random numbers with gaussian distn. with specified variance
gaussiannoise :: RandomizedSignal -> Int -> V.Vector Double
gaussiannoise (RS variance seed) len = V.map ((sqrt variance) *) $ boxmuller (uniformnoise seed len) (uniformnoise (seed+3) len)

-- make various types of signals that our sources could produce

-- sine wave
makeSineSignal :: Double -> SignalMeasurement -> V.Vector Double
makeSineSignal frequency (SM dt start dur) = V.map sinify $ V.enumFromStepN start dt numsteps
               where sinify = (sin .  (2 * pi * frequency *))
                     numsteps = (floor $ dur / dt)

-- gaussian noise signal
makeWhiteSignal :: Int -> RandomizedSignal -> SignalMeasurement -> V.Vector Double
makeWhiteSignal source_seed r@(RS variance random_seed) (SM dt start dur) = (V.++) rn sn
                where rn = gaussiannoise r (floor $ start / dt)
                      sn = gaussiannoise (RS variance source_seed) (floor $ dur / dt)

-- additive white gaussian noise (AWGN) channel model with variance
-- specified applied to signal
noiseifyS :: RandomizedSignal -> Int -> V.Vector Double -> V.Vector Double
noiseifyS r nsteps signal = V.zipWith (+) signal $ gaussiannoise r nsteps

noiseify var = noiseifyS (RS 22 var) 

-- There's a bug here -- all the start delays are twice as long as
-- they should be!
receive :: Int -> Source -> Receiver -> V.Vector Double
receive random_seed (Source src_pos (RS srcvar srcseed)) (Receiver recv_pos rsig@(SM dt start dur)) =
    makeWhiteSignal srcseed (RS srcvar random_seed) (SM dt (start+delay) dur)
    where
      delay = lightdelay src_pos recv_pos
      
-- seed bodge beware!
receivenoisyS :: RandomizedSignal -> Source -> Receiver-> V.Vector Double
receivenoisyS rs@(RS _ seed) src recv@(Receiver pos (SM dt start dur)) = noiseifyS rs (floor $ dur / dt) $ receive (222222-seed) src recv
receivenoisy var = receivenoisyS (RS var 42)

xcorr2 :: V.Vector Double -> V.Vector Double -> Double
xcorr2 va vb = V.sum $ V.zipWith (*) va vb

xcorr3 :: V.Vector Double -> V.Vector Double -> V.Vector Double -> Double
xcorr3 va vb vc = (xcorr2 vb vc) * (xcorr2 va vc) * (xcorr2 va vb)

-- xconv takes a sliding window of width w, starting with va and vb
-- aligned and moving vb backwards in time (so if receiver b is 100
-- units further from the source than receiver a, the 100th element of
-- the covariance vector will spike
xconv :: V.Vector Double -> V.Vector Double -> Int -> V.Vector Double
xconv va vb w = if (w >= (max la lb)) 
                   then V.empty
                   else V.map corr offsets
      where la = V.length va
            lb = V.length vb
            steps = lb - w
            offsets = V.enumFromN 0 steps
            corr offset = xcorr2 va (V.drop offset vb)

-- xconvsym takes xconv in both directions, so it computes negative
-- time offsets as well.
xconvsym :: V.Vector Double -> V.Vector Double -> Int -> (V.Vector Double, V.Vector Double)
xconvsym va vb w = (xconv vb va w, xconv va vb w)

symwindow :: Int -> (V.Vector Double, V.Vector Double) -> V.Vector Double
symwindow width (va, vb) = (V.++) (V.take w va) (V.take w vb)
          where w = width `div` 2

-- xcorrinds takes a single cross-correlation with a window of width w
-- and three vectors.  The second two vectors are offset by the given
-- indices.
xcorrinds :: V.Vector Double -> V.Vector Double -> V.Vector Double -> Int -> Int -> Int -> Double
xcorrinds va vb vc w bi ci = xcorr3 (V.take w va) (V.drop bi vb) (V.drop ci vc)

-- Test data
syncMeasurement = (SM 1 0 1e6)

r_a :: Receiver
r_a = Receiver (Vector3 0 0 0) syncMeasurement
r_b :: Receiver
r_b = Receiver (Vector3 10 0 0) syncMeasurement
r_c :: Receiver
r_c = Receiver (Vector3 0 20 0) syncMeasurement

s_a :: Source
s_a = Source (Vector3 0 0 0) (RS 0.05 1)

rcv_a = receivenoisyS (RS 1.0 2) s_a r_a
rcv_b = receivenoisyS (RS 1.0 3) s_a r_b
rcv_c = receivenoisyS (RS 1.0 4) s_a r_c

plotsignal name len signal = plotPath [Title name] $ V.toList $ V.zip (V.enumFromN 0 len) $ V.take len signal

plot2dcorr name w s r1 r2 r3 = plotFunc3d [Title name] [Plot3dType Surface] [0..s] [0..s] (xcorrinds r1 r2 r3 w)


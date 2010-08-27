> module RandomSignals where

> import System.Random
> import qualified Data.Vector.Storable as V

> import Objects

create stream of uniformly distributed random number on [0,1)

> uniformnoise :: Int -> Int -> [Double]
> uniformnoise seed len = take len $ randoms $ mkStdGen seed

Create stream of gaussian distributed random numbers applying a
Box-Muller transform to an input stream of uniformly distributed
random numbers

> boxmuller :: Int -> Int -> V.Vector Double
> boxmuller seed len = (V.++) (V.fromList v1) (V.fromList v2)
>            where bm (x, y) = ((bmc x y), (bms x y))
>                  bmc x y = sqrt(-2 * log x) * cos (2 * pi * y)
>                  bms x y = sqrt(-2 * log x) * sin (2 * pi * y)
>                  (v1, v2) = unzip $ map bm $ zip (uniformnoise seed len) (uniformnoise (seed+3) len)

stream of random numbers with gaussian distn. with specified variance

> gaussiannoise :: RandomizedSignal -> Int -> V.Vector Double
> gaussiannoise (RS variance seed) len = V.map ((sqrt variance) *) $ boxmuller seed len

make various types of signals that our sources could produce

sine wave

> makeSineSignal :: Double -> SignalMeasurement -> V.Vector Double
> makeSineSignal frequency (SM dt start dur) = V.map sinify $ V.enumFromStepN start dt numsteps
>                where sinify = (sin .  (2 * pi * frequency *))
>                      numsteps = (floor $ dur / dt)

gaussian noise signal

> makeWhiteSignal :: Int -> RandomizedSignal -> SignalMeasurement -> V.Vector Double
> makeWhiteSignal source_seed r@(RS variance random_seed) (SM dt start dur) = (V.++) rn sn
>                 where rn = gaussiannoise r (floor $ start / dt)
>                       sn = gaussiannoise (RS variance source_seed) (floor $ dur / dt)

additive white gaussian noise (AWGN) channel model with variance
specified applied to signal

> noiseifyS :: RandomizedSignal -> Int -> V.Vector Double -> V.Vector Double
> noiseifyS r nsteps signal = V.zipWith (+) signal $ gaussiannoise r nsteps

> noiseify var = noiseifyS (RS 22 var) 

There's a bug here -- all the start delays are twice as long as
they should be!

> receive :: Int -> Source -> Receiver -> V.Vector Double
> receive random_seed (Source src_pos (RS srcvar srcseed)) (Receiver recv_pos rsig@(SM dt start dur)) =
>     makeWhiteSignal srcseed (RS srcvar random_seed) (SM dt (start+delay) dur)
>     where
>       delay = lightdelay src_pos recv_pos
      
seed bodge beware!

> receivenoisyS :: RandomizedSignal -> Source -> Receiver-> V.Vector Double
> receivenoisyS rs@(RS _ seed) src recv@(Receiver pos (SM dt start dur)) = noiseifyS rs (floor $ dur / dt) $ receive (222222-seed) src recv
> receivenoisy var = receivenoisyS (RS var 42)


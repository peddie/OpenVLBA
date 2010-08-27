> module Objects where

Positions

> data V3 = V3 { x :: Double 
>              , y :: Double
>              , z :: Double 
>              } deriving (Show, Eq)

> vsub :: V3 -> V3 -> V3
> vsub (V3 a1 a2 a3) (V3 b1 b2 b3) = V3 (a1-b1) (a2-b2) (a3-b3)

> mag :: V3 -> Double
> mag (V3 x y z) = sqrt $ sum $ map (^2) [x, y, z]

> distance :: V3 -> V3 -> Double
> distance a b = mag $ a `vsub` b

speed of light

> c = 1 -- 3e8

time taken for light to get from a to b

> lightdelay a b = (distance a b) / c

SignalMeasurements have a sample time, a (globally referenced,
e.g. GPS) start time and a sample duration.  

> data SignalMeasurement = SM Double Double Double deriving (Show, Eq)

a reciever has a V3 position and a SignalMeasurement at that
position

> data Receiver = Receiver V3 SignalMeasurement deriving (Show, Eq)

a source has a V3 position, a source_seed which should be
unique to the source and a variance (ie. intensity)

> data Source = Source V3 RandomizedSignal deriving (Show, Eq)

RandomizedSignals simply have a variance and a seed.  

> data RandomizedSignal = RS Double Int deriving (Show, Eq)

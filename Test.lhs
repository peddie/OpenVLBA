> module Test where

> import RandomSignals
> import Objects
> import Data.Vector.Storable as V

Test data

> syncMeasurement = (SM 1 0 1e4)

> r_a :: Receiver
> r_a = Receiver (V3 0 0 0) syncMeasurement
> r_b :: Receiver
> r_b = Receiver (V3 10 0 0) syncMeasurement
> r_c :: Receiver
> r_c = Receiver (V3 0 20 0) syncMeasurement

> s_a :: Source
> s_a = Source (V3 0 0 0) (RS 0.05 1)

> rcv_a = receivenoisyS (RS 1.0 2) s_a r_a
> rcv_b = receivenoisyS (RS 1.0 3) s_a r_b
> rcv_c = receivenoisyS (RS 1.0 4) s_a r_c

> mkDelta offset len = (V.++) ((V.++) (V.replicate (offset-1) 0.0) (V.singleton 1.0)) (V.replicate (len - offset) 0.0)

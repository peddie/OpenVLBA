> module NaiveConvolution where

> import qualified Data.Vector.Storable as V

> xcorr2 :: V.Vector Double -> V.Vector Double -> Double
> {-# INLINE xcorr2 #-}
> xcorr2 va vb = V.sum $ V.zipWith (*) va vb

> xcorr3 :: V.Vector Double -> V.Vector Double -> V.Vector Double -> Double
> {-# INLINE xcorr3 #-}
> xcorr3 va vb vc = (xcorr2 vb vc) * (xcorr2 va vc) * (xcorr2 va vb)

xconv takes a sliding window of width w, starting with va and vb
aligned and moving vb backwards in time (so if receiver b is 100
units further from the source than receiver a, the 100th element of
the covariance vector will spike

> xconv :: V.Vector Double -> V.Vector Double -> Int -> V.Vector Double
> xconv va vb w = if (w >= (max la lb)) 
>                    then V.empty
>                    else V.map corr offsets
>       where la = V.length va
>             lb = V.length vb
>             steps = lb - w
>             offsets = V.enumFromN 0 steps
>             corr offset = xcorr2 va (V.drop offset vb)

xconvsym takes xconv in both directions, so it computes negative
time offsets as well.

> xconvsym :: V.Vector Double -> V.Vector Double -> Int -> (V.Vector Double, V.Vector Double)
> xconvsym va vb w = (xconv vb va w, xconv va vb w)

> symwindow :: Int -> (V.Vector Double, V.Vector Double) -> V.Vector Double
> symwindow width (va, vb) = (V.++) (V.take w va) (V.take w vb)
>           where w = width `div` 2

xcorrinds takes a single cross-correlation with a window of width w
and three vectors.  The second two vectors are offset by the given
indices.

> xcorrinds :: V.Vector Double -> V.Vector Double -> V.Vector Double -> Int -> Int -> Int -> Double
> {-# INLINE xcorrinds #-}
> xcorrinds va vb vc w bi ci = xcorr3 (V.take w va) (V.drop bi vb) (V.drop ci vc)


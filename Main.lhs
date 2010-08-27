OpenVLBA is the Open Very Long Baseline Array.  This project develops
open-source long-baseline interferometry for radio astronomy.

Copyright (C) 2010 Fergus Noble, Matt Peddie {fnoble, mpeddie}@gmail.com

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.


OpenVLBA main function.  Currently just plots some cross-correlations.  

> module Main where

> import System.Random
> import qualified Data.Vector.Storable as V
> import Graphics.Gnuplot.Simple

> import NaiveConvolution
> import RandomSignals
> import Objects
> import Test

> main = do
>        plot2dcorr "damnit" 1000 100 rcv_a rcv_b rcv_c

> plotsignal name len signal = plotPath [Title name] $ V.toList $ V.zipWith (\x y -> (x,y)) (V.enumFromN 0 len) $ V.take len signal

> plot2dcorr name w s r1 r2 r3 = plotFunc3d [Title name] [Plot3dType Surface] [0..s] [0..s] (xcorrinds r1 r2 r3 w)


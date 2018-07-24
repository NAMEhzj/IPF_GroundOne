> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE GADTs #-}
>  
>
> module Main where
>
> import System.IO
> import System.Random
>
> import Intervals.IntervalType 
>
> import Graphics
> import VisualData
> import IPFCore
> import SampleGen
>
>
> dim7 = typeSeven
> dim5 = typeFive
> dim4 = typeFour
> dim3 = typeThree 
> dim2 = typeTwo
>
> myBnds2 :: (Vect (S (S Z)) Int, Vect (S (S Z)) Int)
> myBnds2 = ((0:#(0:#Nil)),(9:#(9:#Nil)))
>
> myBnds3 :: (Vect (S (S (S Z))) Int, Vect (S (S (S Z))) Int)
> myBnds3 = ((0:#(0:#(0:#Nil))), (4:#(4:#(4:#Nil)))) 
>
> myBnds4 :: (Vect (S (S (S (S Z)))) Int, Vect (S (S (S (S Z)))) Int)  
> myBnds4 = ((0:#(0:#(0:#(0:#Nil)))), (2:#(3:#(4:#(5:#Nil)))))
>
> 
> myBnds5 :: (Vect (S (S (S (S (S Z))))) Int, Vect (S (S (S (S (S Z))))) Int)
> myBnds5 = ((0:#(0:#(0:#(0:#(0:#Nil))))), (5:#(6:#(2:#(4:#(3:#Nil))))))
>
> graphDimension4 = FS (FS (FS F0))
> graphDimension2 = FS F0
> 
> genDrawIPF :: IO()
> genDrawIPF = do
>                gen0 <- getStdGen
>                let dim = dim5
>                let (intData, gen1) = randomData dim myBnds5 gen0
>                let intervalData = mapData (double2Interval.fromIntegral) intData
>                let history = historyIPF dim intervalData (intToNat 10)
>                let visualData = map (ivMDBoundDeviations graphDimension4) history
>                let path = "IPFGraphResults/MultiDim/test1.tex"
>                buildGraphicTwo path visualData 
>                putStrLn(show(42))
> 
>
> 
> main :: IO()
> main = genDrawIPF
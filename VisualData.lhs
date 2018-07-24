> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE GADTs #-}
> 
>
> module VisualData where
>
> import Data.Array
>
> import Intervals.IntervalType
> import Intervals.IntervalOps
> 
> import IPFTools
>
>
> ivMDBoundDeviations :: (Fin (S n)) -> IPFData (S n) Interval -> [(Double, Double)]
> ivMDBoundDeviations d (mx, mgs) = map deviation $ zip (splitAlongDim d mx) (vGet d mgs)
>	                       
>
>
> deviation :: ((Array (Vect n Int) Interval), Interval) -> (Double, Double)
> deviation (mx, mgInt) = ((sum (map lb (elems mx)) - mg)/mg, (sum (map rb (elems mx)) - mg)/mg) where
>                                  mg = cen mgInt
>
> 
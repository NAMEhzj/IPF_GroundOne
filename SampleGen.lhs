> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE GADTs #-}
> 
> module SampleGen where
> 
> import System.Random
> import Data.Array
>
> import IPFTools
>
> -- generates a random vector in the given bounds, and also returns the random generator
>
> randomEntry :: (Vect n Int, Vect n Int) -> StdGen -> (Vect n Int, StdGen)
> randomEntry (Nil , Nil)     baseGen = (Nil, baseGen)
> randomEntry ((x:#v),(y:#w)) baseGen = ((newNum:#oldNums), newGen) where
> 											(oldNums, oldGen) = randomEntry (v, w) baseGen
>										  	(newNum, newGen)  = randomR (x, y) oldGen  		
>
> -- generates a list of random vectors in the given bounds, the length of a list will be the given natural number,
> -- also returns the random generator
>
> randomEntries :: (Vect n Int, Vect n Int) -> StdGen -> Nat -> ([Vect n Int], StdGen)
> randomEntries bounds baseGen Z     = ([], baseGen)
> randomEntries bounds baseGen (S k) = (newEntrty : oldEntries, newGen) where
>                                        (oldEntries, oldGen) = randomEntries bounds baseGen k
>                                   	   (newEntrty, newGen)  = randomEntry bounds oldGen
>
> -- adds an integer to a (n-dimentional) matrix/tensor of integers at a given entry, if that entry is within the bounds of the matrix
>
> put :: Int -> Array (Vect n Int) Int -> Vect n Int -> Array (Vect n Int) Int
> put x mx v = array (bounds mx) newAssocs where
>                         newAssocs      = map maybePut (assocs mx)
>                         maybePut (w,z) | w == v    = (w, z+x)
>                                        | otherwise = (w, z)			
>
> -- randomly adds the given number to different places in the matrix.
>
> sprinkle :: Int -> ((Array (Vect n Int) Int), StdGen) -> ((Array (Vect n Int) Int), StdGen)
> sprinkle x (mx, g) = (foldl (put x) mx entries, newGen) where
>				(entries, newGen) = randomEntries (bounds mx) g (intToNat (9*(rangeSize (bounds mx))))
>
>
> -- sprinkles differently sized values on a matrix whose entries are all 1 at the start. 
> -- in the end the average value of a cell is 1000
>
> randomMatrix1 :: (Vect n Int, Vect n Int) -> StdGen -> ((Array (Vect n Int) Int), StdGen)
> randomMatrix1 bounds gen = sprinkle 1 $ sprinkle 10 $ sprinkle 100 ((listArray bounds [1,1..]), gen)
>
>
> -- creates a list of random integers between 1 and 2000, it length is given by the first argument.
>
> randomValues :: Int -> StdGen -> ([Int], StdGen)
> randomValues n baseGen | n<=0      = ([], baseGen)
>                        | otherwise = (newVal:oldVals, newGen) where
>                              (oldVals, oldGen) = randomValues (n-1) baseGen 
>                              (newVal, newGen)  = randomR (1, 2000) oldGen
>
>
> -- creates a matrix with completely random values between 1 and 2000, in this function the will not be
> -- a guaranteed average of 1000, but it will on average be roughly 1000 
>
> randomMatrix2 :: (Vect n Int, Vect n Int) -> StdGen -> ((Array (Vect n Int) Int), StdGen)
> randomMatrix2 bounds gen = (listArray bounds values, newGen) where
>                               (values, newGen) = randomValues (rangeSize bounds) gen 
>
> -- creates a random marginal 
>
> randomMarginal :: (Vect (S n) Int, Vect (S n) Int) -> StdGen -> Fin (S n) -> (Array (Vect (S Z) Int) Int, StdGen)
> randomMarginal (lb, ub) gen d = sprinkle (1*otherdims) $ sprinkle (10*otherdims) $ 
>                                    sprinkle (100*otherdims) $ sprinkle (1000*otherdims) $ 
>                                    ((listArray ((0:#Nil), ((thisDim - 1):#Nil)) [otherdims, otherdims..]), gen) where
>                                       thisDim   = rangeSize (vGet d lb, vGet d ub)
>                                       otherdims = div (rangeSize (lb, ub)) thisDim 
>
> -- creates all marginals, randomly.
>
> randomMarginals :: (Vect n Int, Vect n Int) -> StdGen -> TypeNat n -> Vect k (Fin n) -> ((Vect k [Int]), StdGen)
> randomMarginals bounds baseGen t Nil         = (Nil, baseGen)
> randomMarginals bounds baseGen (TS t) (i:#is) = (((elems newMarg) :# oldMargs), newGen) where
>                                                 (oldMargs, oldGen) = randomMarginals bounds baseGen (TS t) is
>                                                 (newMarg, newGen)  = randomMarginal bounds oldGen i                                                 
>
> -- generates a random set of IPFData (i. e. a matrix with marginals)
> 
> randomData :: TypeNat n -> (Vect n Int, Vect n Int) -> StdGen -> (IPFData n Int, StdGen)
> randomData t bounds gen0 = ((matrix, marginals), gen2) where
>                                 (matrix, gen1)    = randomMatrix2 bounds gen0
>                                 (marginals, gen2) = randomMarginals bounds gen1 t (allFin t)
>

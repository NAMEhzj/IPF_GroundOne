> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE StandaloneDeriving #-}
>
> module BasicTools where
> -- this is iterative proportional fitting for more than just two dimensions (any dimension, as long as you manage to get
> -- an element of TypeNat for that number
>
> import Data.Array
>
> -- First, a few Simple types to act like we can do dependent typing
> -- good old Nat
> data Nat = Z | S Nat
> 
> intToNat :: Int -> Nat
> intToNat n | n <= 0    = Z
>            | otherwise = S (intToNat (n - 1))
>
> natToInt :: Nat -> Int
> natToInt Z     = 0
> natToInt (S n) = 1 + natToInt n
>
> -- natural numbers up to n (with DataKinds)
> data Fin :: Nat -> * where
>            F0 :: Fin (S n)
>            FS :: Fin n -> Fin (S n)
> 
> -- just a type that depends on a natural but has only one element per number, this is to wrap the number of dimensions
> -- into a type, so that we can handle different numbers in different ways using pattern matching
> data TypeNat :: Nat -> * where
>                  T0 :: TypeNat Z
>                  TS :: TypeNat n -> TypeNat (S n)
>
> -- unfortunately we need some kind of interface to actually get a type from a number, but since that is
> -- fundamentally impossible in haskell to do for a general number, here are just some we might use:
> typeZero = T0
> typeOne = TS typeZero
> typeTwo = TS typeOne
> typeThree = TS typeTwo
> typeFour = TS typeThree
> typeFive = TS typeFour
> typeSix = TS typeFive
> typeSeven = TS typeSix
> typeEight = TS typeSeven
> typeNine = TS typeEight
> typeTen = TS typeNine
>
>
> -- a little Function to create a list of all elements in (Fin n), uses n disguised as a type in TypeNat n
> allFin :: (TypeNat n) -> Vect n (Fin n)
> allFin T0     = Nil
> allFin (TS i) = F0 :# (fmap FS (allFin i))
> 
> -- a Vector with fixed length n:
> data Vect :: Nat -> * -> * where
>            Nil  :: Vect Z a
>            (:#) :: a -> Vect n a -> Vect (S n) a 
>            
> deriving instance Show a => Show (Vect n a) 
>  
> 
> toList :: Vect n a -> [a]
> toList Nil = []
> toList (x:#v) = x:(toList v)
>
> -- instances for Eq, Ord and Ix to use these Vectors as array indicies, all implemented similarly to how they are implemented for tuples in haskell
> instance (Eq a) => Eq (Vect n a) where
>             Nil    == Nil    = True
>             (x:#u) == (y:#v) = (x==y) && (u == v)
>
> instance (Ord a) => Ord (Vect n a) where
>              Nil    <= Nil    = True
>              (x:#u) <= (y:#v) | x > y     = False
>                               | x < y     = True
>                               | otherwise = (u <= v)
>
> instance (Ix a) => Ix (Vect n a) where
>                     range (Nil, Nil)      = [Nil]
>                     range ((x:#u),(y:#v)) = concat [ map (z:#) (range (u,v))| z <- range (x,y)]
>                     
>                     inRange (Nil, Nil) Nil          = True
>                     inRange ((x:#u), (y:#v)) (z:#w) = (inRange (x,y) z) && (inRange (u,v) w)
>
>                     index (Nil, Nil) Nil          = 0
>                     index ((x:#u), (y:#v)) (z:#w) = (index (x,y) z) * (rangeSize (u,v)) + (index (u,v) w)
>
>                     rangeSize (Nil, Nil)      = 1
>                     rangeSize ((x:#u),(y:#v)) = (rangeSize (x,y)) * (rangeSize (u,v))
> 
>
>
> 
> -- some very handy operations on these Vectors:
> 
> -- get the k-th entry (k given by an onject of Fin n, so it will always be a valid coordinate!)
> vGet :: (Fin n) -> (Vect n a) -> a
> vGet F0     (x:#_) = x
> vGet (FS i) (_:#u) = vGet i u 
> 
> -- set the k-th entry to a different value 
> vSet :: (Fin n) -> a -> (Vect n a) -> (Vect n a)
> vSet F0     y (_:#u) = (y:#u)
> vSet (FS i) y (x:#u) = (x:#(vSet i y u))  
> 
> 
> -- drop the k-th entry of the vector so you are left with a vector of smaller size
> vDrop :: (Fin (S n)) -> (Vect (S n) a) -> (Vect n a)
> vDrop F0     (_:#u)        = u    
> vDrop (FS i) (x:#u@(_:#_)) = (x:#(vDrop i u))
>
> -- insert a new value into the vector to create a vector of bigger size.
> vInsert :: (Fin (S n)) -> a -> (Vect n a) -> (Vect (S n) a)
> vInsert F0     y v      = (y:#v)
> vInsert (FS i) y (x:#u) = (x:#(vInsert i y u)) 
>
> -- nice Properties: for all k in (Fin n), l in (Fin (S n)), x,y in a, u in Vect n a, v in Vect (S n) a 
> -- vGet k (vSet k x u)      = x
> -- vSet k y (vSet k x u)    = vSet k y u
> -- vDrop l (vSet l x v)     = vDrop k u
> -- vGet l (vInsert l x u)   = x 
> -- vSet l y (vInsert l x u) = vInsert l y u
> -- vDrop l (vInsert l x u)  = u
> -- et cetera
>
> 
> vectFoldr :: (a -> b -> b) -> b -> Vect n a -> b
> vectFoldr _ b Nil    = b
> vectFoldr f b (x:#v) = f x (vectFoldr f b v)
>
> instance Functor (Vect n) where
>            fmap f Nil = Nil
>            fmap f (x:#v) = (f x):#(fmap f v)
>
>
> vect2ToPair :: Vect (S (S Z)) a -> (a, a)
> vect2ToPair (x:#(y:#Nil)) = (x, y)
> 
> pairToVect2 :: (a, a) -> Vect (S (S Z)) a
> pairToVect2 (x,y) = (x:#(y:#Nil))
>
>
> -- a useful function to work with bounds: 
> pairmap :: (a -> b) -> (a, a) -> (b, b)
> pairmap f (x,y) = (f x, f y) 


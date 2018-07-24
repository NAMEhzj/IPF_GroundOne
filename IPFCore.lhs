> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE GADTs #-}
> 
>
> module IPFCore (module IPFCore, module IPFTools) where
> -- this is iterative proportional fitting for n dimensions (any dimension n, as long as you manage to get
> -- an element of TypeNat for that number
> 
> import Data.Array
> import IPFNum
> import IPFTools
>
> 
> -- analogue to adaptrow and adaptcol functions in twoDim IPF: instead of summing over 1-dimensional 
> -- subpsaces we sum over n-1 dimensional subspcaes (Hypersurfaces) and adapt entries to the marginal value 
> adaptSurf :: IPFNum a => ((Array (Vect n Int) a), a) -> (Array (Vect n Int) a)
> adaptSurf (mx, mrg) = fmap (\x -> x * (divide mrg (sum (elems mx)))) mx
>
>
> -- adapts the Dataset in one dimension, basically just doing a split followed by an adaptSurf on all the slices and then a join
> -- also makes sure all the marginals are where they should be
> adaptIn1Dim :: IPFNum a => (Fin (S n)) -> (IPFData (S n) a) -> (IPFData (S n) a)
> adaptIn1Dim d (mx, mrgs) = ((joinAlongDim d).(map adaptSurf) $ (zip (splitAlongDim d mx) (vGet d mrgs)), mrgs)
> 
> -- adapts in every possible direction. This procedure is definitely dependent on the number of dimensions though,
> -- so we need TypeNat at this point, to know what all the different dimensions are (given by allFin)
> oneIPF :: IPFNum a => (TypeNat n) -> (IPFData n a) -> (IPFData n a)
> oneIPF T0       dataset = dataset
> oneIPF t@(TS _) dataset = foldr adaptIn1Dim dataset (toList $ allFin t)
>
> -- iterates IPF and writes down the result of every iteration (including both the starting end ending dataset 
> historyIPF :: IPFNum a => (TypeNat n) -> (IPFData n a) -> Nat -> [IPFData n a]
> historyIPF t dataset Z     = [dataset]
> historyIPF t dataset (S n) = dataset : (historyIPF t (oneIPF t dataset) n)
> 
> 

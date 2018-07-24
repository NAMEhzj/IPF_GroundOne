> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE GADTs #-}
>
> module IPFTools (module IPFTools, module BasicTools) where 
>
> import Data.Array
> import BasicTools
>
> -- to tidy up the code, here a tpye synonym for what we are usually working with: 
> -- a big matrix together with a list of marginals for every dimension
> type IPFData n a = ((Array (Vect n Int) a), (Vect n [a]))
> 
> mapData :: (a -> b) -> IPFData n a ->  IPFData n b 
> mapData f (matrix, marginals) = (fmap f matrix, fmap (fmap f) marginals)
>
> -- split the big array into its hypersyrfaces along the k-th dimension, to be adapted with adaptSurf
> splitAlongDim :: (Fin (S n)) -> (Array (Vect (S n) Int) a) -> [Array (Vect n Int) a] 
> splitAlongDim d mx = [listArray newBounds [mx!u | u <- range $ pairmap (vSet d i) (bounds mx)] | i <- range dimBounds]
>                                      where dimBounds = pairmap (vGet d) (bounds mx)  -- bounds for selected Dimension 
>                                            newBounds = pairmap (vDrop d) (bounds mx) -- bounds for the hypersurfaces
> 
> -- reconstruct the big array from its hypersurfaces along the k-th dimension. Since the old bounds in that direction are lost,
> -- the new ones will just start from 0 and go to however many surfaces there are (minus 1 since we started at 0)
> joinAlongDim :: (Fin (S n)) -> [Array (Vect n Int) a] -> (Array (Vect (S n) Int) a)
> joinAlongDim d mxs = array fullBounds $ concat [map (addIndex i) (assocs (mxs!!i)) | i <- [0..(length mxs - 1)]]
>                                                where fullBounds       = (vInsert d 0 lb, vInsert d (length mxs - 1) ub)
>                                                      (lb,ub)          = bounds (mxs!!0)
>                                                      addIndex i (v, x)= (vInsert d i v, x) 
>

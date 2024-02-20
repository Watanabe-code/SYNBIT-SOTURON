lookup :: ([(Int,Int)],Int) -> Int
lookup t = case t of
             ((k1,v1) : rest , k) -> case k of
                                       k' | (k = k1)     -> v1
                                       k' | not (k = k1) -> lookup (rest,k)
             ([],k)               -> 0

not :: Bool -> Bool
not True = False
not False = True

deleteNat :: Nat -> BX Nat -> BX ()
deleteNat n bm = case n of
  Z -> case* bm of 
         Z -> ()
              with (\x1 -> True)
              reconciled by (\x0 -> \x1 -> x0)
  S n' -> let* (S m') = bm in deleteNat n' m'

(deletNat 2) n 
= case n of 
    S x1 -> case x1 of
              S x0 -> case x0 of
                        Z -> ()
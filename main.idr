module Main

import Data.Fin

data Vect : Nat -> Type -> Type where
    Nil : Vect Z a
    (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

%name Vect xs, ys, zs

total recMin : Nat -> Nat -> Nat
recMin _ Z = Z 
recMin Z _ = Z
recMin (S x) (S y) = S (recMin x y)

-- zip interesting as its not the natural Haskell definition
zip1 : Vect n1 a -> Vect n2 b -> Vect (recMin n1 n2) (a, b)
zip1 _ Nil = the (Vect 0 (a, b)) Nil
zip1 (x :: xs) (y :: ys) = (x, y) :: zip1 xs ys

main : Bool -> IO ()
main False = if Prelude.List.index 2 [1, 2, 3] > 7 then pure () else pure ()
main True = pure ()
module Chapter3

import Data.Vect

createEmpties : Vect n (Vect 0 a)
createEmpties = replicate _ []

transposeMat : Vect n (Vect m a) -> Vect m (Vect n a)
transposeMat [] = createEmpties
transposeMat (x :: xs) = zipWith (::) x (transposeMat xs)

-- addMatrix [[1, 2], [3, 4]] [[5, 6], [7, 8]]
addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix = zipWith (zipWith (+))

dot : Num a => Vect n a -> Vect n a -> a
dot l r = foldl (+) 0 $ zipWith (*) l r

-- multMatrix [[1, 2], [3, 4], [5, 6]] [[7, 8, 9, 10], [11, 12, 13, 14]]
multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect q a) -> Vect n (Vect q a)
multMatrix l r = map (\row => map (dot row) (transposeMat r)) l
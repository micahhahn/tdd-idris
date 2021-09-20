module Chapter5

import Data.Vect

readVect : IO (n ** Vect n String)
readVect = do
    x <- getLine
    if (x == "")
        then pure (_ ** [])
        else do (_ ** xs) <- readVect
                pure (_ ** x :: xs)

zipInputs : IO ()
zipInputs = do
    (n1 ** v1) <- readVect
    (n2 ** v2) <- readVect
    case exactLength n1 v2 of
        Nothing => putStrLn "Not matching"
        Just v2' => if v1 == v2'
                        then putStrLn "Equal"
                        else putStrLn "Unequal"

exactLength' : (n : Nat) -> Vect m a -> Maybe (Vect n a)
exactLength' Z [] = Just []
exactLength' Z _ = Nothing
exactLength' (S k) [] = Nothing
exactLength' (S k) (x :: xs) = map (x ::) $ exactLength' k xs
module DataStore

import Data.String
import Data.Vect

data DataStore : Type where
    MkData : { size : Nat } -> (items : Vect size String ) -> DataStore

size0 : DataStore -> Nat
size0 (MkData { size } _) = size

items : (store : DataStore) -> Vect (size0 store) String
items (MkData items) = items
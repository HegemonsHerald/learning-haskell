-- This is me coming up with Logic-encoding types without knowing what I'm doing!

-- SINCE RECORDS ARE SUMMED UP PRODUCT TYPES DEFINING A LOGICAL COMPUTATION AS
-- A TYPE SHOULD BE EMBEDABBLE IN A RECORD DEFINTION.



data Bool' value = True' value | False' value deriving (Show)

-- And data  constructor :: (* -> *) -> (* -> *) -> * -> *
-- And value constructor :: lb v -> rb v -> v -> And lb rb v
data And value = And (Bool' value) (Bool' value) value

-- data <left> = <right>
-- <left> is the data or type constructor -- a function, that makes a new type
--  if the type is polymorphic, ie higher-kinded it takes other types as type
--  arguments to <left>
-- <right> is the value constructor -- a structural description of where which
--  TYPE is used, where all types have to evaluate to concrete types, so type
--  constructors have to be supplied with all arguments. Note, that this is
--  not directly a function, it's a construcor name followed by a list of
--  concrete types: And <slot1 :: Bool' v> <slot2 :: Bool' v> <slot3 :: v>
-- The constructor function(s) is(/are) derived from the value constructor's
--  description of the type.

instance (Show v) => Show (And v) where
   show (And (False' x) (False' y) z) = "False' " ++ (show z)
   show (And (False' x) (True'  y) z) = "False' " ++ (show z)
   show (And (True'  x) (False' y) z) = "False' " ++ (show z)
   show (And (True'  x) (True'  y) z) = "True' "  ++ (show z)

data Or v = Or (Bool' v) (Bool' v) v

instance (Show v) => Show (Or v) where
   show (Or (False' x) (False' y) z) = "False' " ++ (show z)
   show (Or (False' x) (True'  y) z) = "True' "  ++ (show z)
   show (Or (True'  x) (False' y) z) = "True' "  ++ (show z)
   show (Or (True'  x) (True'  y) z) = "True' "  ++ (show z)

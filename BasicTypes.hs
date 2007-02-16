
module BasicTypes
where
import Utils


type Date = String
type Status = Bool

-- amounts
-- amount arithmetic currently ignores currency conversion

data Amount = Amount {
                      currency :: String,
                      quantity :: Double
                     } deriving (Eq,Ord)

instance Num Amount where
    abs (Amount c q) = Amount c (abs q)
    signum (Amount c q) = Amount c (signum q)
    fromInteger i = Amount "$" (fromInteger i)
    (+) = amountAdd
    (-) = amountSub
    (*) = amountMult
Amount ca qa `amountAdd` Amount cb qb = Amount ca (qa + qb)
Amount ca qa `amountSub` Amount cb qb = Amount ca (qa - qb)
Amount ca qa `amountMult` Amount cb qb = Amount ca (qa * qb)

instance Show Amount where show = amountRoundedOrZero

amountRoundedOrZero :: Amount -> String
amountRoundedOrZero (Amount cur qty) =
    let rounded = printf "%.2f" qty in
    case rounded of
      "0.00"    -> "0"
      "-0.00"   -> "0"
      otherwise -> cur ++ rounded

-- generic tree. each node is a tuple of the node type and a
-- list of subtrees
newtype Tree a = Tree { node :: (a, [Tree a]) } deriving (Show,Eq)
branches = snd . node


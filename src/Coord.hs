module Coord
    where

import Data.Sequence (Seq, Seq (..))
import qualified Data.Sequence as Seq

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Applicative (liftA2)
import Test.QuickCheck

data Zone = Zone Integer Integer 
    deriving (Show,Eq)
data Coord = Coord Integer Integer 
    deriving (Show,Eq)

data Direction = H | B | G | D deriving (Show,Eq)

data Mouvement = Mouv Direction Integer 
    deriving (Show,Eq)

instance Arbitrary Mouvement where
    arbitrary = do
        d <-(arbitrary :: Gen Direction) 
        i <-(arbitrary :: Gen Integer)
        return $ Mouv d i

--instance (Num a, Ord a) => Eq (Coord a a) where
--    (==) (Coord x1 y1) (Coord x2 y2) = x1==x2 && y1==y2
instance Arbitrary Direction where
    arbitrary = frequency [  (25, return H),
                            (25, return B),
                            (25, return G),
                            (25, return D)] 
instance Arbitrary Coord where
   arbitrary = do
     Positive x <- arbitrary
     Positive y <- arbitrary
     return $ Coord x y

bougeCoord :: Coord -> Mouvement -> Coord
bougeCoord (Coord x y) (Mouv H v) = (Coord x (y-v))
bougeCoord (Coord x y) (Mouv B v) = (Coord x (y+v))
bougeCoord (Coord x y) (Mouv G v) = (Coord (x-v) y)
bougeCoord (Coord x y) (Mouv D v) = (Coord (x+v) y)

--bougeCoordSafe ( (Coord 5 3), (Mouv G 10), (Zone 30 30))


bougeCoordSafe :: Coord -> Mouvement -> Zone -> Maybe Coord
bougeCoordSafe c m (Zone h l) = let ( Coord x y) = (bougeCoord c m) in 
    if y>=0 && y<=h && x>=0 && x<=l then Just (Coord x y)
    else Nothing

--Proprieté qui verifie que bouger à droite puis à gauche revient à rester à la même position
prop_gaucheDroite_bougeCoord :: Coord -> Integer -> Bool 
prop_gaucheDroite_bougeCoord c v = bougeCoord (bougeCoord c (Mouv G v)) (Mouv D v)  == c

-- >>> prop_gaucheDroite_bougeCoord (Coord 5 3) 6


module Hitbox 
    where

import Coord

import Data.Sequence (Seq, Seq (..))
import qualified Data.Sequence as Seq

import Data.Set (Set)
import qualified Data.Set as Set
import Test.QuickCheck


data Hitbox = Rect Coord Integer Integer 
            | Composite (Seq Hitbox) 
            deriving (Show,Eq)


genHitbox2 :: Gen Hitbox
genHitbox2 = do
    g <- (arbitrary :: Gen (Seq (Coord, Coord)))
    return (construireHitbox g)

instance Arbitrary Hitbox where
    arbitrary = genHitbox2

-- Invariant qui vérifie qu'une hitbox ne peut pas être vide
prop_Inv_Hitbox :: Hitbox -> Bool
prop_Inv_Hitbox (Composite s) = not(null s) 
prop_Inv_Hitbox _ = True


makeRect :: Coord -> Coord -> Hitbox
makeRect (Coord x1 y1) (Coord x2 y2)  = ( Rect (Coord (min x1 x2) (min y1 y2)) (abs $ x1-x2) (abs $ y1-y2))

construireHitbox :: Seq (Coord, Coord) -> Hitbox
construireHitbox Empty = (Rect (Coord 1 1) 2 3) -- utilisé pour quickCheck mais normalement on ne donne pas de Seq vide
construireHitbox (( c1, c2 ) :<| Empty) = makeRect c1 c2
construireHitbox (( c1, c2 ) :<| cx) = (Composite ( (makeRect c1 c2):<|(aux cx) ))


aux :: Seq (Coord, Coord) -> Seq Hitbox
aux (( c1, c2 ) :<| Empty) = (makeRect c1 c2):<|Empty
aux (( c1, c2 ) :<| cx) = (makeRect c1 c2):<|(aux cx) 

c1= Coord 3 8
c2 = Coord 1 4
c3 = Coord 25 8
c4 = Coord 8 3 
test = Seq.fromList [(c1,c2),(c3,c4)]


-- >>> construireHitbox test
-- Composite (fromList [Rect (Coord 1 4) 2 4,Rect (Coord 8 3) 17 5])
--

-- >>> construireHitbox (Seq.fromList [(c1,c2)])
-- Rect (Coord 1 4) 2 4
--


--if (Coord x1 y1)==(Coord x2 y2) 
--    then Nothing 
--    else Just


appartient :: Coord -> Hitbox -> Bool
appartient ( Coord x y ) (Rect (Coord xr yr) h l) = x>=xr && x<=(xr+l) && y>=yr && y<=(yr+h)
appartient c (Composite Empty ) = False
appartient c (Composite (r:<|rs) ) = if appartient c r then True else appartient c (Composite rs)


-- >>>appartient (Coord 80 80) (Rect (Coord 30 30) 100 100 )
-- True
--

-- >>>construireHitbox (Seq.fromList [(( Coord 30 30), (Coord (130) (130)))])
-- Rect (Coord 30 30) 100 100
--



box1= construireHitbox test

bougeHitbox :: Hitbox -> Mouvement -> Hitbox
bougeHitbox (Rect c h l ) m = (Rect (bougeCoord c m ) h l)
bougeHitbox (Composite rs) m = Composite ( fmap (\r -> bougeHitbox r m) rs )

-- >>> bougeHitbox box1 (Mouv H 2)
-- Composite (fromList [Rect (Coord 1 2) 2 4,Rect (Coord 8 1) 17 5])
--

-- Post-condition qui verifie qu'une fois qu'on a fait appel à bougeHitx certaines coordonnées n'appartiennent plus à la hitbox
prop_post_bougeHitbox :: Hitbox -> Mouvement -> Bool
prop_post_bougeHitbox _ m@(Mouv _ 0) = True
prop_post_bougeHitbox r@(Rect c h l) m@(Mouv H i) = not $ appartient (bougeCoord c (Mouv B h)) $ bougeHitbox r m
prop_post_bougeHitbox r@(Rect c h l) m@(Mouv B i) = not $ appartient (bougeCoord c (Mouv H h)) $ bougeHitbox r m
prop_post_bougeHitbox r@(Rect c h l) m@(Mouv G i) = not $ appartient (bougeCoord c (Mouv D l)) $ bougeHitbox r m
prop_post_bougeHitbox r@(Rect c h l) m@(Mouv D i) = not $ appartient (bougeCoord c (Mouv G h)) $ bougeHitbox r m
prop_post_bougeHitbox (Composite (r:<|Empty)) m = (prop_post_bougeHitbox r m)
prop_post_bougeHitbox (Composite (r:<|rs)) m = (prop_post_bougeHitbox r m) && (prop_post_bougeHitbox (Composite rs) m)


inZoneH :: Hitbox -> Zone -> Bool
inZoneH (Rect (Coord x y) h l) (Zone h1 l1) = x >= 0 && (x+l)<= (l1) && y>=0 && (y+h)<=h1
inZoneH (Composite (r:<|rs)) z = (inZoneH r z ) && (inZoneH (Composite rs) z)

bougeHitboxSafe :: Hitbox -> Mouvement -> Zone -> Maybe Hitbox
bougeHitboxSafe r@(Rect c h l) m z = if inZoneH (bougeHitbox r m) z then Just (bougeHitbox r m) else Nothing
bougeHitboxSafe (Composite rs) m z = fmap Composite $ traverse (\r -> bougeHitboxSafe r m z) rs



-- >>> box1
-- Composite (fromList [Rect (Coord 1 4) 2 4,Rect (Coord 8 3) 17 5])
--

-- >>> bougeHitboxSafe (Rect (Coord 10 10) 20 20) (Mouv H 100) (Zone 200 200)
-- Just (Rect (Coord 10 110) 20 20)
--

-- >>> bougeHitboxSafe box1 (Mouv H 100) (Zone 200 200)
-- Just (Composite (fromList [Rect (Coord 1 104) 2 4,Rect (Coord 8 103) 17 5]))
--


-- >>> bougeHitboxSafe (Composite (Seq.fromList [Rect (Coord 1 4) 2 4,Rect (Coord 8 3) 17 5, Rect (Coord 12 96) 1 0])) (Mouv H 100) (Zone 200 200)
-- Just (Composite (fromList [Rect (Coord 1 104) 2 4,Rect (Coord 8 103) 17 5,Rect (Coord 12 196) 1 0]))
--



collision :: Hitbox -> Hitbox -> Bool
collision h1 h2 = aux_collision h1 h2 || aux_collision h2 h1

aux_collision :: Hitbox -> Hitbox -> Bool
aux_collision (Rect p@(Coord x y) h l) hit = 
    (appartient (Coord x y) hit) || 
    (appartient (Coord (x+l) y) hit) || 
    (appartient (Coord x (y+h)) hit) || 
    (appartient (Coord (x+l) (y+h)) hit)
aux_collision (Composite (r:<|Empty)) hit = aux_collision r hit
aux_collision (Composite (r:<|rs)) hit = aux_collision r hit || aux_collision (Composite rs) hit



-- >>>construireHitbox (Seq.fromList [(Coord 12 15, Coord 23 36), (Coord 62 65, Coord 78 35)])
-- Composite (fromList [Rect (Coord 12 15) 11 21,Rect (Coord 62 35) 16 30])
--

-- >>> collision (Rect (Coord 30 30) 100 100 ) (Rect (Coord 20 20) 100 0 )
-- False
--

-- >>> collision (Rect (Coord 70 43) 100 100 ) (Rect (Coord 62 35) 16 30)
-- True
--

-- >>> collision (Rect (Coord 70 43) 100 100 ) (construireHitbox (Seq.fromList [(Coord 12 15, Coord 23 36), (Coord 62 65, Coord 78 35)]))
-- True
--

-- >>> collision (construireHitbox (Seq.fromList [(Coord 12 15, Coord 23 36), (Coord 62 65, Coord 78 35)])) (Rect (Coord 70 43) 100 100 )
-- True
--

-- >>> collision (construireHitbox (Seq.fromList [(Coord 12 15, Coord 23 36), (Coord 62 65, Coord 78 35) ])) (construireHitbox (Seq.fromList [(Coord 1000 1000, Coord 23 36), (Coord 70 43, Coord 170 143) ]))
-- True
--


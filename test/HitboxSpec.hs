module HitboxSpec where

import Test.Hspec
import Test.QuickCheck

import Hitbox
import Coord
import System.Random (Random)
import Control.Applicative (liftA2)
import Data.Sequence (Seq, Seq (..))
import qualified Data.Sequence as Seq
import Data.List


-- Générateur de Hitbox
genHitbox :: Gen Hitbox
genHitbox = do
    x <- choose (0, 200)
    y <- choose (0, 300)
    h <- choose (0,300)
    l <- choose (0,200)
    return (Rect (Coord x y) h l)




property_inv_hitbox :: Property
property_inv_hitbox = forAll genHitbox2 $ prop_Inv_Hitbox

property_post_bougeHitbox :: Property
property_post_bougeHitbox = property $ prop_post_bougeHitbox

makeHitboxInv = do
    describe " création hitbox" $ do
        it "invariant préservé" $ property property_inv_hitbox
        it " post-condition pour bougeHitbox" $ property property_post_bougeHitbox


-- Generateur de Mouvement

genMouv :: Gen Mouvement
genMouv = frequency [ (25,  (do
                                                m <- choose (1,50)
                                                return (Mouv H m)) ),
                      (25,  (do
                                                m <- choose (1,50)
                                                return (Mouv B m)) ),
                      (25,  (do
                                                m <- choose (1,50)
                                                return (Mouv G m)) ),
                      (25,  (do
                                                m <- choose (1,50)
                                                return (Mouv D m)) )                          
                    ]
--property_post_bougeHitbox :: Property
--property_post_bougeHitbox = forAll genHitbox2 $ \h -> prop_post_bougeHitbox h (Mouv d i)
  
samples ::  Gen a -> IO a
samples gen = do
  l <- sample' gen
  return $ Data.List.last l

-- >>> Test.QuickCheck.generate (arbitrary :: Gen (Seq (Coord, Coord)))
-- fromList [(Coord 2 11,Coord 24 21),(Coord 18 11,Coord 29 25),(Coord 1 21,Coord 3 19),(Coord 21 12,Coord 18 21),(Coord 5 23,Coord 8 27),(Coord 5 22,Coord 7 18),(Coord 26 17,Coord 20 21),(Coord 30 11,Coord 22 12),(Coord 18 18,Coord 24 16),(Coord 19 15,Coord 12 15),(Coord 20 30,Coord 8 16),(Coord 15 28,Coord 29 5),(Coord 24 3,Coord 25 13),(Coord 18 10,Coord 16 17),(Coord 1 27,Coord 18 20),(Coord 18 6,Coord 18 23),(Coord 21 15,Coord 2 22),(Coord 5 25,Coord 1 9),(Coord 20 13,Coord 29 13),(Coord 22 14,Coord 9 21)]
--

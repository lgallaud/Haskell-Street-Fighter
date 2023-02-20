module CoordSpec where

import Test.Hspec
import Test.QuickCheck

import Coord



-- Générateur de coordonnées
genCoord :: Gen Coord
genCoord = do
    x <- choose (0, 200)
    y <- choose (0, 300)
    return $ Coord x y
    
property_bougeGD :: Property 
property_bougeGD = forAll genCoord $ prop_gaucheDroite_bougeCoord 
    
coordSpecGD = do
    describe " bouger à gauche puis à droite" $ do
        it " GD = aucun changement de coord " $ property property_bougeGD


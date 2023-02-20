module CombatSpec
    where
        
import Test.Hspec
import Test.QuickCheck

import Model_Combat
import Coord
import Hitbox

comb1 = Comb (Coord 200 500) (Rect (Coord 200 500) 150 150) D (Ok 5) 0 0

comb2 = Comb (Coord 600 500) (Rect (Coord 600 500) 150 150) G (Ok 5) 0 0

jeu = EnCours comb1 comb2 (Zone 1200 720)

property_post_setEtat :: EtatCombattant -> Combattant -> Property
property_post_setEtat e c = property $ prop_post_setEtatComb e c

property_post_setStun :: Integer -> Combattant -> Property
property_post_setStun i c = property $ prop_post_setStunComb i c

property_post_setInv :: Integer -> Combattant -> Property
property_post_setInv i c = property $ prop_post_setInvComb i c

property_post_rendInv1 ::  Integer -> Jeu -> Property
property_post_rendInv1  i j = property $ prop_post_rendInvincible 1 i j

property_post_rendStun1:: Integer -> Jeu -> Property
property_post_rendStun1 i j = property $ prop_post_rendStun 1 i j

property_pre_perdVie :: Jeu -> Property
property_pre_perdVie j = property $ prop_pre_perdVie 1 j

property_post_perdVie :: Jeu -> Property
property_post_perdVie j = property $ prop_post_perdVie 1 j

property_inv_Position :: Property
property_inv_Position = property $ prop_inv_Position

property_inv_Chevauchement :: Property
property_inv_Chevauchement = property $ prop_inv_Chevauchement

property_post_tourJeu :: Property
property_post_tourJeu = property $ prop_post_tourJeu

property_post_bougeJoueur :: Jeu -> Mouvement -> Property
property_post_bougeJoueur j m = property $ prop_post_bougeJoueur 1 j m

property_pre_passeEtatCoup :: Jeu -> Property
property_pre_passeEtatCoup j = property $ prop_pre_passeEtatCoup 1 j

property_pre_passeEtatSaut :: Jeu -> Property
property_pre_passeEtatSaut j = property $ prop_pre_passeEtatSaut 1 j

property_post_passeEtatCoup :: Jeu -> Property
property_post_passeEtatCoup j = property $ prop_post_passeEtatCoup 1 j

property_post_passeEtatSaut :: Jeu -> Property
property_post_passeEtatSaut j = property $ prop_post_passeEtatSaut 1 j


setTest = do 
   describe "Test des propriétés de jeu, combattant et fonctions" $ do
        it "Test changement d'etat du combattant" $
            property property_post_setEtat
        it "Test changement du parametre stun du combattant" $
            property property_post_setStun
        it "Test changements du parametre d'invincibilité du combattant" $
            property property_post_setInv
        it "Test rendre invincible un combattant" $
            property property_post_rendInv1
        it "Test rendre bloqué un combattant" $
            property property_post_rendStun1
        it "Test pré condition perte de vie d'un combattant"$
            property property_pre_perdVie
        it "Test post condition perte de vie d'un combattant"$
            property property_pre_perdVie
        it " preservation invariant position des combattants dans le jeu" $
            property property_inv_Position 
        it " preservation invariant pas de chevauchements des combattants dans le jeu" $
            property property_inv_Position 
        it " Teste si un des joueurs est Ko alors le jeu est terminé"$
            property property_post_tourJeu
        it " Teste qu'un joueur ne peut pas bouger en dehors de la zone"$
            property $ property_post_bougeJoueur
        it "Test pre condition pour passer à l'etat Saut"$
            property $ prop_pre_passeEtatSaut
        it "Test pre condition pour passer à l'etat Coup"$
            property $ prop_pre_passeEtatCoup
        it "Test post condition pour passer à l'etat Saut"$
            property $ prop_post_passeEtatSaut
        it "Test post condition pour passer à l'etat Coup"$
            property $ prop_post_passeEtatCoup


        

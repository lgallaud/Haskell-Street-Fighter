module Model_Combat
     where

import Data.Sequence (Seq, Seq (..))
import qualified Data.Sequence as Seq

import Control.Applicative (liftA2)
import Test.QuickCheck

import Data.Set (Set)
import qualified Data.Set as Set
import Coord
import Hitbox
import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K

data EtatCombattant = Ko | Ok Integer deriving (Show,Eq)

genEtat :: Gen EtatCombattant
genEtat = frequency [ (15,  (do
                                                m <- choose (1,6)
                                                return (Ok m))) ,
                      (85,  return Ko)] 

instance Arbitrary EtatCombattant where
    arbitrary = genEtat

genComb :: Gen Combattant
genComb = frequency [(20, (do 
                                    c <- (arbitrary :: Gen Coord)
                                    h <- (arbitrary :: Gen Hitbox)
                                    d <- (arbitrary :: Gen Direction)
                                    e <- (arbitrary :: Gen EtatCombattant)
                                    s <- choose(1,10)
                                    i <- choose(1,10)
                                    return (Comb c h d e s i))),
                    (20, ( do       
                                    c <- (arbitrary :: Gen Coord)
                                    h <- (arbitrary :: Gen Hitbox)
                                    d <- (arbitrary :: Gen Direction)
                                    e <- (arbitrary :: Gen EtatCombattant)
                                    f <- choose(1,10)
                                    s <- choose(1,10)
                                    i <- choose(1,10)
                                    return (CombCoup c h d e f s i))),
                    (20, ( do       
                                    c <- (arbitrary :: Gen Coord)
                                    h <- (arbitrary :: Gen Hitbox)
                                    d <- (arbitrary :: Gen Direction)
                                    e <- (arbitrary :: Gen EtatCombattant)
                                    f <- choose(1,10)
                                    s <- choose(1,10)
                                    i <- choose(1,10)
                                    return (CombSaut c h d e f s i))),
                    (20, ( do       
                                    c <- (arbitrary :: Gen Coord)
                                    h <- (arbitrary :: Gen Hitbox)
                                    d <- (arbitrary :: Gen Direction)
                                    e <- (arbitrary :: Gen EtatCombattant)
                                    f <- choose(1,10)
                                    s <- choose(1,10)
                                    i <- choose(1,10)
                                    return (CombCoup2 c h d e f s i))),
                    (20, ( do       
                                    c <- (arbitrary :: Gen Coord)
                                    h <- (arbitrary :: Gen Hitbox)
                                    d <- (arbitrary :: Gen Direction)
                                    e <- (arbitrary :: Gen EtatCombattant)
                                    f <- choose(1,10)
                                    s <- choose(1,10)
                                    i <- choose(1,10)
                                    return (CombBloque c h d e f s i)))
                                    ]
instance Arbitrary Combattant where
    arbitrary = genComb

data Combattant = Comb {
    positionc :: Coord,
    hitboxc :: Hitbox,
    facec :: Direction,
    etatx :: EtatCombattant,
    stunFrame :: Integer,
    invincFrame :: Integer
} | CombCoup {
    positionc :: Coord,
    hitboxc :: Hitbox,
    facec :: Direction,
    etatx :: EtatCombattant,
    frameRestante :: Integer,
    stunFrame :: Integer,
    invincFrame :: Integer
} | CombCoup2 {
    positionc :: Coord,
    hitboxc :: Hitbox,
    facec :: Direction,
    etatx :: EtatCombattant,
    frameRestante :: Integer,
    stunFrame :: Integer,
    invincFrame :: Integer
}| CombSaut {
    positionc :: Coord,
    hitboxc :: Hitbox,
    facec :: Direction,
    etatx :: EtatCombattant,
    frameRestante :: Integer,
    stunFrame :: Integer,
    invincFrame :: Integer
} | CombBloque{
    positionc :: Coord,
    hitboxc :: Hitbox,
    facec :: Direction,
    etatx :: EtatCombattant,
    frameRestante :: Integer,
    stunFrame :: Integer,
    invincFrame :: Integer
} deriving (Show, Eq)

data Jeu = GameOver Integer
        | EnCours {
            joueur1 :: Combattant,
            joueur2 :: Combattant,
            zoneJeu :: Zone
        }
        deriving (Show,Eq)

genJeu :: Gen Jeu
genJeu = frequency [
    (15, ( do 
                i <- choose(1,2)
                return (GameOver i)
        )),
    (85, ( do
                c1 <- (arbitrary :: Gen Combattant)
                c2 <- (arbitrary :: Gen Combattant)
                h <- choose(200,800)
                l <- choose(600,1800)
                return (EnCours c1 c2 (Zone l h))))]

instance Arbitrary Jeu where
    arbitrary = genJeu

getCombX :: Combattant -> Integer
getCombX (Comb (Coord x y) _ _ _ _ _) = x
getCombX (CombCoup (Coord x y) _ _ _ _ _ _) = x
getCombX (CombCoup2 (Coord x y) _ _ _ _ _ _) = x
getCombX (CombSaut (Coord x y) _ _ _ _ _ _) = x
getCombX (CombBloque (Coord x y) _ _ _ _ _ _) = x

getCombY :: Combattant -> Integer
getCombY (Comb (Coord x y) _ _ _ _ _) = y
getCombY (CombCoup (Coord x y) _ _ _ _ _ _) = y
getCombY (CombCoup2 (Coord x y) _ _ _ _ _ _) = y
getCombY (CombSaut (Coord x y) _ _ _ _ _ _) = y
getCombY (CombBloque (Coord x y) _ _ _ _ _ _) = y

getCoordComb :: Combattant -> Coord
getCoordComb (Comb c _ _ _ _ _) = c
getCoordComb (CombCoup c _ _ _ _ _ _) = c
getCoordComb (CombCoup2 c _ _ _ _ _ _) = c
getCoordComb (CombSaut c _ _ _ _ _ _) = c
getCoordComb (CombBloque c _ _ _ _ _ _) = c

getHitBoxComb :: Combattant -> Hitbox
getHitBoxComb (Comb _ h _ _ _ _) = h
getHitBoxComb (CombCoup _ h _ _ _ _ _) = h
getHitBoxComb (CombCoup2 _ h _ _ _ _ _) = h
getHitBoxComb (CombSaut _ h _ _ _ _ _) = h
getHitBoxComb (CombBloque _ h _ _ _ _ _) = h

getDirComb :: Combattant -> Direction
getDirComb (Comb _ _ d _ _ _) = d
getDirComb (CombCoup _ _ d _ _ _ _) = d
getDirComb (CombCoup2 _ _ d _ _ _ _) = d
getDirComb (CombSaut _ _ d _ _ _ _) = d
getDirComb (CombBloque _ _ d _ _ _ _) = d

getEtatComb :: Combattant -> EtatCombattant
getEtatComb (Comb _ _ _ e _ _) = e
getEtatComb (CombCoup _ _ _ e _ _ _) = e
getEtatComb (CombCoup2 _ _ _ e _ _ _) = e
getEtatComb (CombSaut _ _ _ e _ _ _) = e
getEtatComb (CombBloque _ _ _ e _ _ _) = e

setEtatComb :: EtatCombattant -> Combattant -> Combattant
setEtatComb e (Comb c h d x i inf) = (Comb c h d e i inf)
setEtatComb e (CombCoup c h d x f i inf) = (CombCoup c h d e f i inf)
setEtatComb e (CombCoup2 c h d x f i inf) = (CombCoup2 c h d e f i inf)
setEtatComb e (CombSaut c h d x f i inf) = (CombSaut c h d e f i inf)
setEtatComb e (CombBloque c h d x f i inf) = (CombBloque c h d e f i inf)


--Post-condition pour la fonction setEtatComb qui vérifie que l'etat du combattant a bien été modifié
prop_post_setEtatComb :: EtatCombattant -> Combattant -> Bool
prop_post_setEtatComb e comb@(Comb c h d x i inf) = case setEtatComb e comb of
    (Comb c h d e i inf)-> True
    _ -> False
prop_post_setEtatComb e comb@(CombCoup c h d x f i inf) = case setEtatComb e comb of
    (CombCoup c h d e f i inf) -> True
    _ -> False
prop_post_setEtatComb e comb@(CombCoup2 c h d x f i inf) = case setEtatComb e comb of
    (CombCoup2 c h d e f i inf) -> True
    _ -> False
prop_post_setEtatComb e comb@(CombSaut c h d x f i inf) = case setEtatComb e comb of
    (CombSaut c h d e f i inf) -> True
    _ -> False
prop_post_setEtatComb e comb@(CombBloque c h d x f i inf) = case setEtatComb e comb of
    (CombBloque c h d e f i inf) -> True
    _ -> False


-- >>> prop_post_setEtatComb (Ko) j1
-- True
--
estInvincible :: Combattant -> Bool
estInvincible (Comb _ _ _ _ _ i) = i>0
estInvincible (CombCoup _ _ _ _ _ _ i) = i>0
estInvincible (CombCoup2 _ _ _ _ _ _ i) = i>0
estInvincible (CombSaut _ _ _ _ _ _ i) = i>0
estInvincible (CombBloque _ _ _ _ _ _ i) = i>0

setStunComb :: Integer -> Combattant -> Combattant
setStunComb e (Comb c h d x i inf) = (Comb c h d x e inf)
setStunComb e (CombCoup c h d x f i inf) = (CombCoup c h d x f e inf)
setStunComb e (CombCoup2 c h d x f i inf) = (CombCoup2 c h d x f e inf)
setStunComb e (CombSaut c h d x f i inf) = (CombSaut c h d x f e inf)
setStunComb e (CombBloque c h d x f i inf) = (CombBloque c h d x f e inf)

--Post-condition pour la fonction setStunComb qui vérifie que le nombre de framestun du combattant a bien été modifié
prop_post_setStunComb :: Integer -> Combattant -> Bool
prop_post_setStunComb e comb@(Comb c h d x i inf) = case setStunComb e comb of
    (Comb c h d x e inf)-> True
    _ -> False
prop_post_setStunComb e comb@(CombCoup c h d x f i inf) = case setStunComb e comb of
    (CombCoup c h d x f e inf) -> True
    _ -> False
prop_post_setStunComb e comb@(CombCoup2 c h d x f i inf) = case setStunComb e comb of
    (CombCoup2 c h d x f e inf) -> True
    _ -> False
prop_post_setStunComb e comb@(CombSaut c h d x f i inf) = case setStunComb e comb of
    (CombSaut c h d x f e inf) -> True
    _ -> False
prop_post_setStunComb e comb@(CombBloque c h d x f i inf) = case setStunComb e comb of
    (CombBloque c h d x f e inf) -> True
    _ -> False

setInvComb :: Integer -> Combattant -> Combattant
setInvComb inf2 (Comb c h d e i inf) = (Comb c h d e i inf2)
setInvComb inf2 (CombCoup c h d e f i inf) = (CombCoup c h d e f i inf2)
setInvComb inf2 (CombCoup2 c h d e f i inf) = (CombCoup2 c h d e f i inf2)
setInvComb inf2 (CombSaut c h d e f i inf) = (CombSaut c h d e f i inf2)
setInvComb inf2 (CombBloque c h d e f i inf) = (CombBloque c h d e f i inf2)

--Post-condition pour la fonction setInvComb qui vérifie que le parametre invincFrame  du combattant a bien été modifié
prop_post_setInvComb :: Integer -> Combattant -> Bool
prop_post_setInvComb inf2 comb@(Comb c h d e i inf) = case setInvComb inf2 comb of
    (Comb c h d e i inf2)-> True
    _ -> False
prop_post_setInvComb inf2 comb@(CombCoup c h d e f i inf) = case setInvComb inf2 comb of
    (CombCoup c h d e f i inf2) -> True
    _ -> False
prop_post_setInvComb inf2 comb@(CombCoup2 c h d e f i inf) = case setInvComb inf2 comb of
    (CombCoup2 c h d e f i inf2) -> True
    _ -> False
prop_post_setInvComb inf2 comb@(CombSaut c h d e f i inf) = case setInvComb inf2 comb of
    (CombSaut c h d e f i inf2) -> True
    _ -> False
prop_post_setInvComb inf2 comb@(CombBloque c h d e f i inf) = case setInvComb inf2 comb of
    (CombBloque c h d e f i inf2) -> True
    _ -> False


-- rend invincible le joueur x pour n secondes
rendInvincible :: Integer -> Integer -> Jeu -> Jeu
rendInvincible 1 n j@(EnCours c1 c2 z ) = (EnCours (setInvComb n c1) c2 z) 
rendInvincible 2 n j@(EnCours c1 c2 z ) = (EnCours c1 (setInvComb n c2) z) 

--Post-condition pour la fonction rendInvincible qui vérifie que le joueur x a bien été rendu invincible pendant n sec

prop_post_rendInvincible :: Integer -> Integer -> Jeu -> Bool
prop_post_rendInvincible 1 n j@(EnCours c1 c2 z ) = let c=setInvComb n c1 in case rendInvincible 1 n j of
    (EnCours c c2 z) -> True
    _-> False
prop_post_rendInvincible 2 n j@(EnCours c1 c2 z ) = let c=setInvComb n c2 in case rendInvincible 2 n j of
    (EnCours c1 c  z) -> True
    _-> False
prop_post_rendInvincible _ _ (GameOver i) = True
    


-- rend paralysé le joueur x pour n secondes
rendStun :: Integer -> Integer -> Jeu -> Jeu
rendStun 1 n j@(EnCours c1 c2 z ) = (EnCours (setStunComb n c1) c2 z) 
rendStun 2 n j@(EnCours c1 c2 z ) = (EnCours c1 (setStunComb n c2) z) 
rendStun _ _ j=j

--Post-condition pour la fonction rendStun qui vérifie que le joueur x a bien été paralysé pendant n sec

prop_post_rendStun :: Integer -> Integer -> Jeu -> Bool
prop_post_rendStun 1 n j@(EnCours c1 c2 z ) = let c= setStunComb n c1 in case rendStun 1 n j of
    (EnCours c c2 z) -> True
    _-> False
prop_post_rendStun 2 n j@(EnCours c1 c2 z ) = let c=setStunComb n c2 in case rendStun 2 n j of
    (EnCours c1 c  z) -> True
    _-> False
prop_post_rendStun _ _ (GameOver i) = True
    


getVie :: Combattant -> Integer
getVie c = case getEtatComb c of
    Ko -> 0
    (Ok i) -> i

perdVie ::  Integer -> Jeu -> Jeu
perdVie 1 j@(EnCours c1 c2 z ) = case getEtatComb c1 of
    Ko -> EnCours (setEtatComb Ko c1) c2 z
    (Ok i) -> EnCours (setEtatComb (metKo (Ok (i-1))) c1) c2 z   
perdVie 2 j@(EnCours c1 c2 z ) = case getEtatComb c2 of
    Ko -> EnCours c1 (setEtatComb Ko c2) z
    (Ok i) -> EnCours c1 (setEtatComb (metKo (Ok (i-1))) c2) z

--Pré-condition pour la fonction perdVie qui vérifie qu'un joueur Ko ou qui à 0PV ne peut pas en perdre
prop_pre_perdVie :: Integer -> Jeu -> Bool
prop_pre_perdVie i j@(EnCours c1 c2 z )= if i==1 then getVie c1 >=0 else (if i==2 then getVie c2 >=0 else False)
prop_pre_perdVie _ (GameOver i) = True
--Post-condition de perdVie qui vérifie que le Joueur a bien perdu de la vie
prop_post_perdVie :: Integer -> Jeu -> Bool
prop_post_perdVie i j@(EnCours c1 c2 z )
    | i==1 = let (EnCours c' c'' z2)= perdVie 1 j in (getVie c')==((getVie c1)-1)
    | i==2 = let (EnCours c' c'' z2)= perdVie 2 j in (getVie c'')==((getVie c2)-1)
    | otherwise = False
prop_post_perdVie _  (GameOver i) = True

metKo :: EtatCombattant -> EtatCombattant
metKo (Ok 0) = Ko
metKo  e = e 


estStun :: Combattant -> Bool
estStun (Comb _ _ _ _ e _) = e>0
estStun (CombCoup _ _ _ _ _ e _) = e>0
estStun (CombCoup2 _ _ _ _ _ e _) = e>0
estStun (CombSaut _ _ _ _ _ e _) = e>0
estStun (CombBloque _ _ _ _ _ e _) = e>0

-- replace la direction d par dir
changeDirection :: Integer -> Direction -> Jeu -> Jeu
changeDirection n dir j@(EnCours c1 c2 z )
    | n == 1 = (EnCours (changeDirectionAux c1 dir) c2 z)
    | n == 2 = (EnCours c1 (changeDirectionAux c2 dir) z)
    | otherwise = j


changeDirectionAux :: Combattant -> Direction -> Combattant
changeDirectionAux (Comb c h d e stun inv) dir = (Comb c h dir e stun inv)
changeDirectionAux (CombCoup c h d e f stun inv) dir = (CombCoup c h dir e f stun inv) 
changeDirectionAux (CombCoup2 c h d e f stun inv) dir = (CombCoup2 c h dir e f stun inv) 
changeDirectionAux (CombSaut c h d e f stun inv) dir = (CombSaut c h dir e f stun inv)
changeDirectionAux (CombBloque c h d e f stun inv) dir = (CombBloque c h dir e f stun inv)

inZone :: Coord -> Zone -> Bool
inZone (Coord x y ) (Zone h l ) = y>=0 && y<=h && x>=0 && x<=l



--Invariant du Jeu qui teste si les combattants sont bien dans la zone de jeu
prop_inv_Position :: Jeu -> Bool
prop_inv_Position (GameOver i) = True
prop_inv_Position (EnCours c1 c2 z) = inZone (getCoordComb c1) z && inZone (getCoordComb c2) z



j1= ( Comb (Coord 2 3) box1 G (Ok 0) 0 0)
j2= ( Comb (Coord 3 4) box1 G (Ok 3) 0 0)

-- >>> prop_inv_Position (GameOver 2)
-- True
--

-- >>> prop_inv_Position (EnCours j1 j2 (Zone 20 20))
-- True
--

-- Autre invariant de jeu qui vérifie que les hitbox des combattants de se chevauchent pas
prop_inv_Chevauchement :: Jeu -> Bool
prop_inv_Chevauchement (GameOver i) = True
prop_inv_Chevauchement (EnCours c1 c2 _) = not(collision (getHitBoxComb c1) (getHitBoxComb c2) )


--Post-condition qui vérifie que si un combattant a 0 ou moins de point de vie alors le jeu est terminé
prop_post_tourJeu :: Jeu -> Bool
prop_post_tourJeu (GameOver i) = True
prop_post_tourJeu j@(EnCours c1 c2 _) = aux_prop_post_tourJeu c1 ( jeufini j)  || aux_prop_post_tourJeu c2 (jeufini j) 


aux_prop_post_tourJeu :: Combattant -> Jeu -> Bool
aux_prop_post_tourJeu c j  = case (getEtatComb c) of
    Ko -> case j of
        GameOver _ -> True
        _ -> False
    Ok i -> if i <=0 then case j of
        GameOver _ -> True
        _ -> False
         else True 


-- >>> prop_post_tourJeu (EnCours j1 j2 (Zone 20 20))
-- False
--


bougeJoueur :: Integer -> Jeu -> Mouvement -> Jeu
bougeJoueur i j@(EnCours c1 c2 z ) m@(Mouv d _)
    | i==1 = if (auxbougeJoueur c1 c2 z m) then 
        (EnCours c1{positionc = (bougeCoord (getCoordComb c1) m), hitboxc=(bougeHitbox (getHitBoxComb c1) m)} c2 z) else j
    | i==2 = if (auxbougeJoueur c2 c1 z m )then 
        (EnCours c1 c2{positionc = (bougeCoord (getCoordComb c2) m), hitboxc=(bougeHitbox (getHitBoxComb c2) m)}  z) else j 
    | otherwise = j


-- False = pas bouger
-- True = on peut bouger
auxbougeJoueur :: Combattant -> Combattant -> Zone -> Mouvement -> Bool
auxbougeJoueur c1 c2 z m = 
    case (bougeCoordSafe (getCoordComb c1) m z) of
        Nothing -> False
        Just (Coord x y) -> True && not(collision (bougeHitbox (getHitBoxComb c1) m) (getHitBoxComb c2))

--Post-condition de bougeJoueur qui vérifie que si un mouvement d'un joueur le sort de la zone alors il ne bouge pas
prop_post_bougeJoueur :: Integer -> Jeu -> Mouvement -> Bool
prop_post_bougeJoueur i j@(EnCours c1 c2 z ) m@(Mouv d _)  
    | i==1 = let j2 = (bougeJoueur i j m) in (if not(auxbougeJoueur c1 c2 z m) then j==j2 else True)
    | i==2 = let j2 = (bougeJoueur i j m) in (if not(auxbougeJoueur c2 c1 z m) then j==j2 else True )
    | otherwise = False
prop_post_bougeJoueur _  (GameOver i) _ = True
         
-- >>> bougeJoueur 2 initJeu (Mouv G 50)
-- EnCours {joueur1 = Comb {positionc = Coord 200 500, hitboxc = Rect (Coord 200 500) 150 150, facec = D, etatx = Ok 5, stunFrame = 0, invincFrame = 0}, joueur2 = Comb {positionc = Coord 550 500, hitboxc = Rect (Coord 550 500) 150 150, facec = G, etatx = Ok 5, stunFrame = 0, invincFrame = 0}, zoneJeu = Zone 2280 720}
--

-- >>>bougeJoueur 1 initJeu (Mouv D 50)
-- EnCours {joueur1 = Comb {positionc = Coord 250 500, hitboxc = Rect (Coord 250 500) 150 150, facec = D, etatx = Ok 5, stunFrame = 0, invincFrame = 0}, joueur2 = Comb {positionc = Coord 600 500, hitboxc = Rect (Coord 600 500) 150 150, facec = G, etatx = Ok 5, stunFrame = 0, invincFrame = 0}, zoneJeu = Zone 2280 720}
--


-- >>> initJeu
-- EnCours {joueur1 = Comb {positionc = Coord 200 500, hitboxc = Rect (Coord 200 500) 150 150, facec = D, etatx = Ok 5, stunFrame = 0, invincFrame = 0}, joueur2 = Comb {positionc = Coord 600 500, hitboxc = Rect (Coord 600 500) 150 150, facec = G, etatx = Ok 5, stunFrame = 0, invincFrame = 0}, zoneJeu = Zone 2280 720}
--

initCombattant1 :: Combattant
initCombattant1 = Comb (Coord 200 500) (Rect (Coord 200 500) 150 150) D (Ok 5) 0 0

initCombattant2 :: Combattant
initCombattant2 = Comb (Coord 600 500) (Rect (Coord 600 500) 150 150) G (Ok 5) 0 0

initJeu :: Jeu
initJeu = (EnCours initCombattant1 initCombattant2 (Zone 1280 1020))



moveLeft :: Integer -> Integer -> Jeu -> Jeu
moveLeft i n j = bougeJoueur i j (Mouv G n)

moveRight :: Integer -> Integer -> Jeu -> Jeu
moveRight i n j = bougeJoueur i j (Mouv D n)
                              
moveUp :: Integer -> Integer -> Jeu -> Jeu
moveUp i n j = bougeJoueur i j (Mouv H n)

moveDown :: Integer -> Integer -> Jeu -> Jeu
moveDown i n j = bougeJoueur i j (Mouv B n)


-- Le personnage passe dans l'état en train de donner un coup
passeEtatCoup :: Integer -> Jeu -> Jeu
passeEtatCoup  1 (EnCours (Comb p h d e stun inv) c2 z)  = (EnCours (CombCoup p h d e 100 stun inv) c2 z)  
passeEtatCoup  2 (EnCours c1 (Comb p h d e stun inv) z)  = (EnCours c1 (CombCoup p h d e 100 stun inv) z)
passeEtatCoup _ j = j  

passeEtatCoup2 :: Integer -> Jeu -> Jeu
passeEtatCoup2  1 (EnCours (Comb p h d e stun inv) c2 z)  = (EnCours (CombCoup2 p h d e 100 stun inv) c2 z)  
passeEtatCoup2  2 (EnCours c1 (Comb p h d e stun inv) z)  = (EnCours c1 (CombCoup2 p h d e 100 stun inv) z)
passeEtatCoup2 _ j = j  


-- Pré-condition qui vérifie que le combattant qui veut donner un coup ne soit pas déjà en train de le faire ou en train de sauter
prop_pre_passeEtatCoup ::  Integer -> Jeu -> Bool
prop_pre_passeEtatCoup 1 (EnCours c1 c2 z) = case c1 of 
    (Comb _ _ _ _ _ _) -> True
    _ -> estStun c1
prop_pre_passeEtatCoup 2 (EnCours c1 c2 z) = case c2 of 
    (Comb _ _ _ _ _ _) -> True
    _ -> estStun c2
prop_pre_passeEtatCoup _  (GameOver i) = True
prop_pre_passeEtatCoup _ _ = True

-- Post-condition qui vérifie que si un combattant est bien de type Comb alors il passe en type CombCoup après passeEtatCoup
prop_post_passeEtatCoup :: Integer -> Jeu -> Bool
prop_post_passeEtatCoup 1 j@(EnCours c1 c2 z) = let j2@(EnCours c' c'' z)= (passeEtatCoup 1 j) in  if (prop_pre_passeEtatCoup 1 j) then 
    case c' of
        (Comb _ _ _ _ n _) -> n>0
        --(CombCoup2 _ _ _ _ _ _ _) -> True
        _ -> True
    else j==j2
prop_post_passeEtatCoup 2 j@(EnCours c1 c2 z) = let j2@(EnCours c' c'' z)= (passeEtatCoup 2 j) in  if (prop_pre_passeEtatCoup 1 j) then 
    case c'' of
        (Comb _ _ _ _ n _) -> n>0
        --(CombCoup2 _ _ _ _ _ _ _) -> True
        _ -> True
    else j==j2
prop_post_passeEtatCoup _ (GameOver i) = True
prop_post_passeEtatCoup _ _ = True


peutBouger :: Combattant -> Bool
peutBouger (Comb _ _ _ _ s _) = s<=0
peutBouger (CombSaut _ _ _ _ _ s _) = s<=0
peutBouger (CombCoup _ _ _ _ _ _ _) = False
peutBouger (CombCoup2 _ _ _ _ _ _ _) = False
peutBouger (CombBloque _ _ _ _ _ _ _) = False

passeEtatSaut :: Integer -> Jeu -> Jeu
passeEtatSaut 1 (EnCours (Comb p h d e stun inv) c2 z)  = (EnCours (CombSaut p h d e 99 stun inv) c2 z)  
passeEtatSaut 2 (EnCours c1 (Comb p h d e stun inv) z)  = (EnCours c1 (CombSaut p h d e 99 stun inv) z)
passeEtatSaut _ j = j  

-- Pré-condition qui vérifie que le combattant qui veut sauter ne soit pas déjà en train de le faire ou en train de donner un coup
prop_pre_passeEtatSaut ::  Integer -> Jeu -> Bool
prop_pre_passeEtatSaut 1 (EnCours c1 c2 z) = case (c1) of 
    (Comb _ _ _ (Ok n) 0 _) -> True
    _ -> not(peutSauter c1)
prop_pre_passeEtatSaut 2 (EnCours c1 c2 z) = case c2 of 
    (Comb _ _ _ (Ok n) 0 _) -> True
    _ -> not(peutSauter c2)
prop_pre_passeEtatSaut _ (GameOver i) = True
prop_pre_passeEtatSaut _ j = True

-- Post-condition qui vérifie que si un combattant est bien de type Comb alors il passe en type CombSaut après passeEtatSaut
prop_post_passeEtatSaut :: Integer -> Jeu -> Bool
prop_post_passeEtatSaut 1 j@(EnCours c1 c2 z) = let j2@(EnCours c' c'' z)= (passeEtatSaut 1 j) in  if (prop_pre_passeEtatSaut 1 j) then 
    case c' of
        (Comb _ _ _ _ n _ ) -> n>0
        _ -> True
    else j==j2
prop_post_passeEtatSaut 2 j@(EnCours c1 c2 z) = let j2@(EnCours c' c'' z)= (passeEtatSaut 2 j) in  if (prop_pre_passeEtatSaut 1 j) then 
    case c'' of
        (Comb _ _ _ _ n _) -> n>0
        _ -> True
    else j==j2
prop_post_passeEtatSaut _ (GameOver i) = True
prop_post_passeEtatSaut _ _ = True


peutSauter :: Combattant -> Bool
peutSauter (Comb _ _ _ _ s _) = s<=0
peutSauter _ = False


peutBloquer :: Combattant -> Bool
peutBloquer (Comb _ _ _ _ s _) = s<=0
peutBloquer _ = False


passeEtatBloque :: Integer -> Jeu -> Jeu
passeEtatBloque 1 (EnCours (Comb p h d e stun inv) c2 z)  = (EnCours (CombBloque p h d e 80 stun inv) c2 z)  
passeEtatBloque 2 (EnCours c1 (Comb p h d e stun inv) z)  = (EnCours c1 (CombBloque p h d e 80 stun inv) z)
passeEtatBloque _ j = j 





-- fonction qui change l'état des frames des combattants à chaque tour de boucle
unInstantPasse :: Jeu -> Jeu
unInstantPasse (EnCours c1 c2 z) = EnCours (unInstantPasseAux c1) (unInstantPasseAux c2) z
unInstantPasse (GameOver i) = GameOver i 

unInstantPasseAux :: Combattant -> Combattant
unInstantPasseAux (CombCoup p h d e frest stun inv) = if frest>0 
    then (CombCoup p h d e (frest-1) (stun-1) (inv-1)) 
    else (Comb p h d e (stun-1) (inv-1))
unInstantPasseAux (CombCoup2 p h d e frest stun inv) = if frest>0 
    then (CombCoup2 p h d e (frest-1) (stun-1) (inv-1)) 
    else (Comb p h d e (stun-1) (inv-1))    
unInstantPasseAux c@(CombSaut p h d e frest stun inv) = if frest>0 
    then (CombSaut p h d e (frest-1) (stun-1) (inv-1)) 
    else (if getCombY c >= 500 
        then (Comb p h d e (stun-1) (inv-1))
        else (CombSaut p h d e (frest-1) (stun-1) (inv-1)))
unInstantPasseAux (Comb p h d e stun inv) = (Comb p h d e (stun-1) (inv-1))
unInstantPasseAux (CombBloque p h d e frest stun inv) = if frest>0 
    then (CombBloque p h d e (frest-1) (stun-1) (inv-1))
    else (Comb p h d e (stun-1) (inv-1))
    
    
    
    


gestionSaut :: Jeu -> Jeu 
gestionSaut j@(EnCours c1 c2 z) = gestionSautAux c1 1 $ gestionSautAux c2 2 j
gestionSaut (GameOver i) = GameOver i

gestionSautAux :: Combattant -> Integer -> Jeu -> Jeu
gestionSautAux c@(CombSaut _ _ _ _ frest _ _) n j = if frest >= 50 
    then (moveUp n 5 j) 
    else (if getCombY c <500 then (moveDown n 5 j) else id j)
gestionSautAux _ _ j = id j 


degatCollision :: Jeu -> Jeu
degatCollision j@(EnCours c1 c2 z) = 
    degatCollisionCdp c1 c2 2 $ degatCollisionCdp c2 c1 1 $
    degatCollisionCoup2 c1 c2 2 $ degatCollisionCoup2 c2 c1 1 j 
degatCollision (GameOver i) = GameOver i

degatCollisionCdp :: Combattant -> Combattant -> Integer -> Jeu -> Jeu
degatCollisionCdp c1@(CombCoup _ _ D _ frest _ _) c2 n = 
    if (frest>30 && frest<70 && collision (construireHitbox 
            (Seq.fromList [(( Coord ((getCombX c1)+150) (getCombY c1)), (Coord ((getCombX c1)+200) ((getCombY c1)+50)))]) ) (getHitBoxComb c2))
                  then (if not(degatBloqueCoup D c2) && not(estInvincible c2)
                        then (perdVie n) . (rendInvincible n 60) . (rendStun n 60)
                        else (rendStun n 40) )  
                    else id
degatCollisionCdp c1@(CombCoup _ _ G _ frest _ _) c2 n =
    if (frest>30 && frest<70 && collision (construireHitbox 
            (Seq.fromList [(( Coord (getCombX c1) (getCombY c1)), (Coord ((getCombX c1)-50) ((getCombY c1)+50)))]) ) (getHitBoxComb c2))
                  then (if not(degatBloqueCoup G c2 ) && not(estInvincible c2) 
                        then (perdVie n) . (rendInvincible n 60) . (rendStun n 60)
                        else (rendStun n 40)) 
                    else id
degatCollisionCdp _ _ _ = id 

degatCollisionCoup2 :: Combattant -> Combattant -> Integer -> Jeu -> Jeu
degatCollisionCoup2 c1@(CombCoup2 _ _ D _ frest _ _) c2 n = 
    if (frest>30 && frest<70 && collision (construireHitbox 
            (Seq.fromList [(( Coord ((getCombX c1)+150) ((getCombY c1)+100)), (Coord ((getCombX c1)+200) ((getCombY c1)+50)))]) ) (getHitBoxComb c2))
                  then (if not(degatBloqueCoup D c2) && not(estInvincible c2)
                        then (perdVie n) . (rendInvincible n 60) . (rendStun n 60)
                        else (rendStun n 40) )  
                  else id
degatCollisionCoup2 c1@(CombCoup2 _ _ G _ frest _ _) c2 n =
    if (frest>30 && frest<70 && collision (construireHitbox 
            (Seq.fromList [(( Coord (getCombX c1) ((getCombY c1)+100)), (Coord ((getCombX c1)-50) ((getCombY c1)+50)))]) ) (getHitBoxComb c2))
                  then (if not(degatBloqueCoup G c2 ) && not(estInvincible c2) 
                        then (perdVie n) . (rendInvincible n 60) . (rendStun n 60)
                        else (rendStun n 40))  
                  else id
degatCollisionCoup2 _ _ _ = id 




degatBloqueCoup :: Direction -> Combattant -> Bool
degatBloqueCoup G (CombBloque _ _ D _ _ _ _) = True
degatBloqueCoup D (CombBloque _ _ G _ _ _ _) = True
degatBloqueCoup _ _ = False


jeufini :: Jeu -> Jeu
jeufini j@(EnCours c1 c2 _) = 
    if getEtatComb c1 == Ko 
    then (GameOver 2) else 
    (if getEtatComb c2 == Ko then (GameOver 1) else j)


gameStep :: RealFrac a => Jeu -> Keyboard -> a -> Jeu
gameStep (GameOver i) kbd deltaTime = (GameOver i)
gameStep jeu@(EnCours c1 c2 z ) kbd deltaTime =
  let modif = 
              (unInstantPasse) 
              .
              (degatCollision)      
              .  
              (gestionSaut)
              .
              (if K.keypressed KeycodeLeft kbd && (peutBouger c2)
               then moveLeft 2 4 . (changeDirection 2 G) else id)
              .
              (if K.keypressed KeycodeRight kbd && (peutBouger c2)
               then moveRight 2 4 . (changeDirection 2 D) else id)
              .
              (if K.keypressed KeycodeUp kbd && (peutSauter c2)
               then (passeEtatSaut 2) else id)
              .
              
                (if K.keypressed KeycodeQ kbd && (peutBouger c1)
               then moveLeft 1 4 . (changeDirection 1 G) else id)
              .
              (if K.keypressed KeycodeD kbd && (peutBouger c1)
               then moveRight 1 4 . (changeDirection 1 D) else id)
              .
              (if K.keypressed KeycodeZ kbd && (peutSauter c1)
               then (passeEtatSaut 1) else id)
              .
                          
              
              (if K.keypressed KeycodeF kbd && not(estStun c1) then (passeEtatCoup 1 ) else id)
              .
              (if K.keypressed KeycodeK kbd && not(estStun c2) then (passeEtatCoup 2 ) else id)
              .
              (if K.keypressed KeycodeG kbd && not(estStun c1) then (passeEtatCoup2 1 ) else id)
              .
              (if K.keypressed KeycodeL kbd && not(estStun c2) then (passeEtatCoup2 2 ) else id)
              .
              (if K.keypressed KeycodeH kbd && (peutBloquer c1) then (passeEtatBloque 1 ) else id)
              .
              (if K.keypressed KeycodeM kbd && (peutBloquer c2) then (passeEtatBloque 2 ) else id)
              .
              (jeufini)

                  
              
  in modif jeu

-- >>>construireHitbox (Seq.fromList [(( Coord 200 300), (Coord (200-50) (300+50)))])
-- Rect (Coord 150 300) 50 50
--

-- >>>construireHitbox (Seq.fromList [(( Coord 200 300), (Coord (200+100) (300+50)))]) 
-- Rect (Coord 200 300) 100 50
--

-- >>> collision (Rect (Coord 310 310) 100 50) (Rect (Coord 400 300) 50 50)
-- False
--


-- >>> (Rect (Coord 400 300) 50 100)
-- Rect (Coord 400 300) 50 100
--



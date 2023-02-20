{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless, foldM)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map
import qualified Data.Map as Map
import Data.List (foldl')

import Foreign.C.Types (CInt (..) )

import SDL
import SDL.Time (time, delay)
import SDL.Video.Renderer
import SDL.Vect
import Linear (V4(..))

import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM

import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

import Keyboard (Keyboard)
import qualified Keyboard as K

import Hitbox
import Coord

import qualified Debug.Trace as T

import Model_Combat 
import qualified Model_Combat as M
import GHC.Base (when)

import Data.List (foldl')
import Data.Sequence (Seq, iterateN, index, fromList, singleton, drop, empty, lookup)
import Data.Vector.Storable




loadBackground :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "background") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 1280 720)
  let smap' = SM.addSprite (SpriteId "background") sprite smap
  return (tmap', smap')


loadPerso :: Renderer -> FilePath -> String -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso rdr path id tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId id) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId id) (S.mkArea 0 0 150 150)
  let smap' = SM.addSprite (SpriteId id) sprite smap
  return (tmap', smap')

loadVie :: Renderer -> FilePath -> String -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadVie rdr path id tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId id) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId id) (S.mkArea 0 0 150 50)
  let smap' = SM.addSprite (SpriteId id) sprite smap
  return (tmap', smap')

loadElem :: CInt -> CInt -> Renderer -> FilePath -> String -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadElem  l h rdr path id tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId id) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId id) (S.mkArea 0 0 l h)
  let smap' = SM.addSprite (SpriteId id) sprite smap
  return (tmap', smap')


toVect :: Hitbox -> Data.Vector.Storable.Vector (Rectangle CInt)
toVect hbx  = case hbx of
    Rect (Coord x y) mvx mvy -> Data.Vector.Storable.singleton (Rectangle (P (V2(fromInteger x) (fromInteger y))) (V2 (fromInteger mvy) (fromInteger mvx)))
    Composite xs -> case (Data.Sequence.lookup 0 xs) of
      Just m -> (Data.Vector.Storable.++) (toVect m)  (toVect( Composite (Data.Sequence.drop 1 xs)))
      Nothing -> Data.Vector.Storable.empty

displayHitbox rend hbx = drawRects rend (toVect hbx)


main :: IO ()
main = do
  initializeAll
  window <- createWindow "Minijeu" $ defaultWindow { windowInitialSize = V2 1280 720 }
  renderer <- createRenderer window (-1) defaultRenderer
  
  -- liste d'éléments (Texture ID, FilePath) à charger

  let elementsPerso = [
                      ("perso1D", "assets/perso1D.bmp"), ("perso1G", "assets/perso1G.bmp"), 
                      ("perso2D", "assets/perso2D.bmp"), ("perso2G", "assets/perso2G.bmp"),
                      ("perso1_attaqueD", "assets/perso1_attaqueD.bmp"), 
                      ("perso1_attaqueG", "assets/perso1_attaqueG.bmp"),
                      ("perso2_attaqueD", "assets/perso2_attaqueD.bmp"), 
                      ("perso2_attaqueG", "assets/perso2_attaqueG.bmp"),
                      ("perso1_attaque2D", "assets/perso1_attaque2D.bmp"), 
                      ("perso1_attaque2G", "assets/perso1_attaque2G.bmp"),
                      ("perso2_attaque2D", "assets/perso2_attaque2D.bmp"), 
                      ("perso2_attaque2G", "assets/perso2_attaque2G.bmp"),
                      ("perso1_sautD", "assets/perso1_sautD.bmp"),
                      ("perso1_sautG", "assets/perso1_sautG.bmp"),
                      ("perso2_sautD", "assets/perso2_sautD.bmp"),
                      ("perso2_sautG", "assets/perso2_sautG.bmp"),                     
                      ("perso1_prepD", "assets/perso1_prepD.bmp"),
                      ("perso1_prepG", "assets/perso1_prepG.bmp"),
                      ("perso2_prepD", "assets/perso2_prepD.bmp"), 
                      ("perso2_prepG", "assets/perso2_prepG.bmp"), 
                      ("perso1_finAttaqueD", "assets/perso1_finAttaqueD.bmp"),
                      ("perso1_finAttaqueG", "assets/perso1_finAttaqueG.bmp"),
                      ("perso2_finAttaqueD", "assets/perso2_finAttaqueD.bmp"), 
                      ("perso2_finAttaqueG", "assets/perso2_finAttaqueG.bmp"),
                      ("perso1_bloqueD", "assets/perso1_bloqueD.bmp"),
                      ("perso1_bloqueG", "assets/perso1_bloqueG.bmp"),
                      ("perso2_bloqueD", "assets/perso2_bloqueD.bmp"), 
                      ("perso2_bloqueG", "assets/perso2_bloqueG.bmp"),
                      ("perso1_dead", "assets/perso1_dead.bmp"),
                      ("perso2_dead", "assets/perso2_dead.bmp")                                              
                      ]

  let elementsVie = [("vie0","assets/vie0.bmp"),
                      ("vie1","assets/vie1.bmp"), 
                      ("vie2","assets/vie2.bmp"), 
                      ("vie3","assets/vie3.bmp"), 
                      ("vie4","assets/vie4.bmp"), 
                      ("vie5","assets/vie5.bmp"),
                      ("joueur1","assets/joueur1.bmp"),
                      ("joueur2","assets/joueur2.bmp")                                                
                      ]
  let elemGO = [("go1","assets/GameOver1.bmp"),
                      ("go2","assets/GameOver2.bmp") ]
  
  -- chargement de l'image du fond
  (tmap, smap) <- loadBackground renderer "assets/background.bmp" TM.createTextureMap SM.createSpriteMap
  -- chargement des personnages
  (tmap', smap') <- Control.Monad.foldM (\(t,s) (id, path) -> loadPerso renderer path id t s ) (tmap, smap) elementsPerso
  -- chargement des vies
  (tmap'', smap'') <- Control.Monad.foldM (\(t,s) (id, path) -> loadVie renderer path id t s ) (tmap', smap') elementsVie

  (tmap''', smap''')<- Control.Monad.foldM (\(t,s) (id, path) -> loadElem 700 300 renderer path id t s ) (tmap'', smap'') elemGO
  -- initialisation de l'état du jeu
  let gameState = M.initJeu
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- lancement de la gameLoop

  gameLoop 60 renderer tmap''' smap''' kbd gameState

vieId :: Integer -> String
vieId i = "vie"<>(show i)

vie :: Combattant -> String
vie c ="vie"<>(show (M.getVie c))
-- >>> vieId 3
-- "vie3"
--

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> Jeu -> IO ()
gameLoop frameRate renderer tmap smap kbd jeu = do
  startTime <- time
  events <- pollEvents
  let (kbd', mouse) = K.handleEvents events kbd
  clear renderer
  --- display background
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
  --- display points de vie
  case jeu of 
    M.EnCours _ _ _ ->  do
      S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "joueur1" )smap) 200 10)
      S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "joueur2" )smap) 200 70)
      S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId (vie(M.joueur1 jeu) ))smap) 10 10)
      S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId (vie(M.joueur2 jeu) ))smap) 10 70)
    M.GameOver _ -> do return () 
  
  
  
  
  --- display perso 
  case jeu of
    M.GameOver 1 -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "go1") smap ) 300 300)
    M.GameOver 2 -> S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "go2") smap ) 300 300)
    M.EnCours _ _ _ ->  do {S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (
                                                              case M.joueur2 jeu of 
                                                                M.Comb _ _ D _ _ _ -> SpriteId "perso2D"
                                                                M.Comb _ _ G _ _ _ -> SpriteId "perso2G"
                                                                M.CombCoup _ _ D _ f _ _ -> if f>70 then SpriteId "perso2_prepD" else 
                                                                                      (if f<30 then SpriteId "perso2_finAttaqueD"
                                                                                      else SpriteId "perso2_attaqueD")                                                            
                                                                M.CombCoup _ _ G _ f _ _ -> if f>70 then SpriteId "perso2_prepG" else 
                                                                                      (if f<30 then SpriteId "perso2_finAttaqueG"
                                                                                      else SpriteId "perso2_attaqueG") 
                                                                M.CombCoup2 _ _ D _ f _ _ -> if f>70 then SpriteId "perso2_prepD" else 
                                                                                      (if f<30 then SpriteId "perso2_finAttaqueD"
                                                                                      else SpriteId "perso2_attaque2D")                                                            
                                                                M.CombCoup2 _ _ G _ f _ _ -> if f>70 then SpriteId "perso2_prepG" else 
                                                                                      (if f<30 then SpriteId "perso2_finAttaqueG"
                                                                                      else SpriteId "perso2_attaque2G")                                                             
                                                                M.CombSaut _ _ D _ _ _ _ -> SpriteId "perso2_sautD"
                                                                M.CombSaut _ _ G _ _ _ _ -> SpriteId "perso2_sautG"
                                                                M.CombBloque _ _ D _ _ _ _ -> SpriteId "perso2_bloqueD"
                                                                M.CombBloque _ _ G _ _ _ _ -> SpriteId "perso2_bloqueG"
                                                              ) smap 
                                                              )
  
                                 (fromIntegral (M.getCombX (M.joueur2 jeu) ))
                                 (fromIntegral (M.getCombY (M.joueur2 jeu))));
                                 S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (
                                                              case M.joueur1 jeu of 
                                                                M.Comb _ _ D _ _ _ -> SpriteId "perso1D"
                                                                M.Comb _ _ G _ _ _ -> SpriteId "perso1G"
                                                                M.CombCoup _ _ D _ f _ _ -> if f>70 then SpriteId "perso1_prepD" else 
                                                                                          (if f<30 then SpriteId "perso1_finAttaqueD"
                                                                                          else SpriteId "perso1_attaqueD") 
                                                                M.CombCoup _ _ G _ f _ _ -> if f>70 then SpriteId "perso1_prepG" else 
                                                                                      (if f<30 then SpriteId "perso1_finAttaqueG"
                                                                                      else SpriteId "perso1_attaqueG")
                                                                M.CombCoup2 _ _ D _ f _ _ -> if f>70 then SpriteId "perso1_prepD" else 
                                                                                      (if f<30 then SpriteId "perso1_finAttaqueD"
                                                                                      else SpriteId "perso1_attaque2D") 
                                                                M.CombCoup2 _ _ G _ f _ _ -> if f>70 then SpriteId "perso1_prepG" else 
                                                                                      (if f<30 then SpriteId "perso1_finAttaqueG"
                                                                                      else SpriteId "perso1_attaque2G") 
                                                                M.CombSaut _ _ D _ _ _ _ -> SpriteId "perso1_sautD"
                                                                M.CombSaut _ _ G _ _ _ _ -> SpriteId "perso1_sautG"
                                                                M.CombBloque _ _ D _ _ _ _ -> SpriteId "perso1_bloqueD"
                                                                M.CombBloque _ _ G _ _ _ _ -> SpriteId "perso1_bloqueG" 
                                                              ) smap )
                                 (fromIntegral (M.getCombX (M.joueur1 jeu) ))
                                 (fromIntegral (M.getCombY (M.joueur1 jeu))))
    }
  ---
  --displayHitbox renderer (M.hitboxc(M.joueur1 jeu))
  --displayHitbox renderer (M.hitboxc(M.joueur2 jeu))

  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  -- putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  -- putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
  --- update du game state


  let gameState' = M.gameStep jeu kbd' deltaTime
  ---
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' gameState')

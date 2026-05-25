module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Debug.Trace (trace)
import Control.Monad.State


import Model
import Config (defaultConfig, execGameM, gameLoopM)

import Keyboard (Keyboard, initKeyboard, handleKeyEvent, isKeyDown)

data GameControl = GameControl { 
    keyboard :: Keyboard,
    gsState :: GameState
  }
  deriving Show

initGame :: GameControl
initGame = GameControl {
                  keyboard = initKeyboard,
                  gsState = initGameState
}


data Assets = Assets {
    bgndAsset    :: Picture
  , persoAsset   :: Picture
  , persoInvAsset :: Picture
  , persoSpeedAsset :: Picture
  , perso2Asset    :: Picture
  , perso2InvAsset :: Picture
  , perso2SpeedAsset :: Picture
  , bossAsset      :: Picture
  , ballAsset    :: Picture
  , tearAsset    :: Picture
  , redAsset     :: Picture
  , greenAsset   :: Picture
  , blueAsset    :: Picture
  , yellowAsset  :: Picture
  , lifeAssets   :: [Picture]  
  , gameOverAsset :: Picture
  , healthBonusAsset :: Picture
  , speedBonusAsset :: Picture
  , invBonusAsset :: Picture
  }

render :: Assets -> GameControl -> Picture
render assets (GameControl _ gs) = case persoHitbox (player gs) of
  Model.Rectangle px py pw ph ->
    let gameOver = lost gs
        p1       = player gs
        hp       = persoHealth p1
        inv      = invincibleTimer p1
        sp       = speedyTimer p1
        ll       = livesLeft p1
        sc       = scrollOffset gs
        projs    = projectiles gs
        enns     = enemies gs
        bons     = bonuses gs
        bgnd     = bgndAsset assets
        -- Couloir : navy foncé et trait épais 2px (deuxième Line décalé d'1px).
        wallsPic = case worldWalls gs of
          Composee [MurGauche lps, MurDroit rps] ->
            let navy = makeColorI 8 22 80 255
            in Pictures [ Color navy (Line lps)
                        , Color navy (Translate 1 0 (Line lps))
                        , Color navy (Line rps)
                        , Color navy (Translate 1 0 (Line rps)) ]
          _ -> Blank
        scorePic = Translate (-screenWidth/2 + 10) (screenHeight/2 - 25)
                 $ Scale 0.15 0.15 $ Color white
                 $ Text ("SCORE " ++ show (score gs))
        renderProjectile (Projectile _ (Disque cx cy _) _ t) = Translate cx cy $ case t of
          Bullet -> ballAsset assets
          Tear   -> tearAsset assets
        renderProjectile _ = Blank
        renderEnnemy (Ennemy _ (Disque cx cy _) _ t _ _) = Translate cx cy $ case t of
          Red    -> redAsset assets
          Green  -> greenAsset assets
          Blue   -> blueAsset assets
          Yellow -> yellowAsset assets
          -- Boss : sprite BMP dédié, remplace la composition procédurale historique.
          Boss   -> bossAsset assets
        renderEnnemy _ = Blank
        renderBonus (Bonus (Disque cx cy r) t _) = Translate cx cy $ case t of
          Health           -> healthBonusAsset assets
          Speed            -> speedBonusAsset assets
          Invincibility    -> invBonusAsset assets
          -- pas d'asset dédié : on dessine un cercle Gloss à la place.
          ScrollReverse    -> Color cyan (ThickCircle (r/2) r)
          TripleShotBonus  -> Color orange (ThickCircle (r/2) r)
        renderBonus _ = Blank
    in
      -- Scale 2 2 : on rend à l'échelle native du modèle puis on agrandit pour une fenêtre lisible sur écran Retina.
      Scale 2 2 $
      if gameOver
      then let bg = Pictures [ Translate 0 sc bgnd, Translate 0 (sc+358) bgnd ]
           in Pictures [bg, Translate 0 0 (gameOverAsset assets), scorePic]
      else
        let bg       = Pictures [ Translate 0 sc bgnd, Translate 0 (sc+358) bgnd ]
            projPics = Pictures (map renderProjectile projs)
            ennPics  = Pictures (map renderEnnemy enns)
            bonPics  = Pictures (map renderBonus bons)
            lifePic  = Translate (screenWidth/2-60) (-screenHeight/2 + 20)
                       (lifeAssets assets !! hp)
            -- Barre PV du joueur 2 : symétrique à gauche, sans teinte (sprites déjà colorés).
            lifePicP2 = case player2 gs of
              Just p2 -> Translate (-screenWidth/2 + 60) (-screenHeight/2 + 20)
                           (lifeAssets assets !! persoHealth p2)
              Nothing -> Blank
            livesPic = Pictures
              [ Translate (screenWidth/2 - 100 - 16 * fromIntegral i) (-screenHeight/2 + 20)
                  (Scale 0.4 0.4 (persoAsset assets))
              | i <- [0 .. ll - 1]
              ]
            -- Essais de P2 : rangée miroir à gauche, icône perso2 réduite.
            livesPicP2 = case player2 gs of
              Just p2 -> Pictures
                [ Translate (-screenWidth/2 + 100 + 16 * fromIntegral i) (-screenHeight/2 + 20)
                    (Scale 0.4 0.4 (perso2Asset assets))
                | i <- [0 .. livesLeft p2 - 1]
                ]
              Nothing -> Blank
            -- P1 disparaît visuellement quand playerDead : cohérent avec P2 qui passe à Nothing.
            persoPic
              | playerDead p1 = Blank
              | inv > 0 && inv `mod` 20 < 10 = persoInvAsset assets
              | sp > 0                        = persoSpeedAsset assets
              | otherwise                     = persoAsset assets
            -- P2 : sprite BMP dédié, même logique de bascule inv/speed que P1.
            p2Pic = case player2 gs of
              Just p2 -> case persoHitbox p2 of
                Model.Rectangle px2 py2 pw2 ph2 ->
                  let cx2 = px2 + pw2/2
                      cy2 = py2 + ph2/2
                      inv2 = invincibleTimer p2
                      sp2  = speedyTimer p2
                      perso2Pic
                        | inv2 > 0 && inv2 `mod` 20 < 10 = perso2InvAsset assets
                        | sp2 > 0                         = perso2SpeedAsset assets
                        | otherwise                       = perso2Asset assets
                  in Translate cx2 cy2 perso2Pic
                _ -> Blank
              Nothing -> Blank
        in Pictures [bg, wallsPic, Translate (px + pw/2) (py + ph/2) persoPic, p2Pic, projPics, ennPics, lifePic, lifePicP2, livesPic, livesPicP2, bonPics, scorePic]
  _ -> Blank

handleEvents :: Event -> GameControl -> GameControl
handleEvents ev (GameControl kbd gs) =
  case ev of

    EventKey (SpecialKey KeySpace) Down _ _ -> 
      (GameControl kbd (execState shootM gs))
    
    -- coop : T bascule la présence du joueur 2.
    EventKey (Char 't') Down _ _ ->
      GameControl kbd (togglePlayer2 gs)

    -- coop : tir joueur 2.
    EventKey (Char 'f') Down _ _ ->
      GameControl kbd (execState shootP2M gs)

    EventKey (Char 'r') Down _ _ ->
      GameControl initKeyboard (execState resetGame gs)

    _ ->  GameControl (handleKeyEvent ev kbd) gs

update :: Float -> GameControl -> GameControl
update _ (GameControl kbd gs) = 
  if lost gs then ( GameControl kbd (execState updateScrollM gs) )
    else
    let k1 = isKeyDown (SpecialKey KeyLeft) kbd in
    let k2 = isKeyDown (SpecialKey KeyRight) kbd in
    let k3 = isKeyDown (SpecialKey KeyUp) kbd in
    let k4 = isKeyDown (SpecialKey KeyDown) kbd in
    -- Touches P2 : WSAD (W=haut, A=gauche, S=bas, D=droite), tir conservé sur F.
    let p2L = isKeyDown (Char 'a') kbd in
    let p2R = isKeyDown (Char 'd') kbd in
    let p2U = isKeyDown (Char 'w') kbd in
    let p2D = isKeyDown (Char 's') kbd in


    let keys = [k1, k2, k3, k4]
        gs1 = execGameM defaultConfig gs (gameLoopM keys)
        -- P2 bouge dans le State pur après la boucle GameM (la boucle gère scroll/spawn/etc).
        gs2 = execState (do
                 if p2L then moveLeftP2M  else return ()
                 if p2R then moveRightP2M else return ()
                 if p2U then moveUpP2M    else return ()
                 if p2D then moveDownP2M  else return ()) gs1
    in GameControl kbd gs2


main :: IO ()
main = do
  assets <- Assets
    <$> loadBMP "./assets/background.bmp"
    <*> loadBMP "./assets/player1.bmp"
    <*> loadBMP "./assets/player1_inv.bmp"
    <*> loadBMP "./assets/player1_speedy.bmp"
    <*> loadBMP "./assets/player2.bmp"
    <*> loadBMP "./assets/player2_inv.bmp"
    <*> loadBMP "./assets/player2_speedy.bmp"
    <*> loadBMP "./assets/ennemy_boss.bmp"
    <*> loadBMP "./assets/ball.bmp"
    <*> loadBMP "./assets/tear.bmp"
    <*> loadBMP "./assets/redE.bmp"
    <*> loadBMP "./assets/greenE.bmp"
    <*> loadBMP "./assets/blueE.bmp"
    <*> loadBMP "./assets/yellowE.bmp"
    <*> mapM (\n -> loadBMP $ "./assets/" <> show n <> "life.bmp") ([0..5] :: [Int])
    <*> loadBMP "./assets/gameover.bmp"
    <*> loadBMP "./assets/bonus_health.bmp"
    <*> loadBMP "./assets/bonus_speed.bmp"
    <*> loadBMP "./assets/bonus_invincibility.bmp"

  -- Fenêtre 2x la taille du modèle (1132x716) pour rester lisible sur écran moderne ; le rendu est mis à l'échelle dans `render`.
  play (InWindow "Minijeu" (1132, 716) (10, 10))
       black 60
       initGame
       (render assets)
       handleEvents
       update


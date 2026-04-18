module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Debug.Trace (trace)

import Model

import Keyboard (Keyboard, initKeyboard, handleKeyEvent, isKeyDown)

data GameControl = GameControl { 
    keyboard :: Keyboard,
    state :: GameState
  }
  deriving Show

initGame :: GameControl
initGame = GameControl {
                  keyboard = initKeyboard,
                  state = initGameState
}


data Assets = Assets {
    bgndAsset    :: Picture
  , persoAsset   :: Picture
  , persoInvAsset :: Picture
  , persoSpeedAsset :: Picture
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
render assets (GameControl _ (GameState gameOver (Player _ (Model.Rectangle px py pw ph) hp inv sp) projs enns _ sc bons _ _)) =

  if gameOver 
  then let bg       = Pictures [ Translate 0 sc bgnd, Translate 0 (sc+358) bgnd ]
       in Pictures [bg,  Translate 0 0 (gameOverAsset assets)]
   else

    let bg       = Pictures [ Translate 0 sc bgnd, Translate 0 (sc+358) bgnd ]
        projPics = Pictures (map renderProjectile projs)
        ennPics  = Pictures (map renderEnnemy enns)
        bonPics  = Pictures (map renderBonus bons)
        lifePic = Translate (screenWidth/2-60) (-screenHeight/2 + 20) 
                (lifeAssets assets !! hp)
        persoPic = if inv > 0 && inv `mod` 20 < 10
            then persoInvAsset assets
            else if sp > 0
                 then persoSpeedAsset assets
                 else persoAsset assets
    in Pictures [bg, Translate (px + pw/2) (py + ph/2) persoPic, projPics, ennPics, lifePic, bonPics]
    where
      bgnd = bgndAsset assets
      renderProjectile (Projectile _ (Disque cx cy r) _ t) = Translate cx cy $ case t of
        Bullet -> ballAsset assets
        Tear   -> tearAsset assets
      renderEnnemy (Ennemy _ (Disque cx cy r) _ t _ _) = Translate cx cy $ case t of
        Red    -> redAsset assets
        Green  -> greenAsset assets
        Blue   -> blueAsset assets
        Yellow -> yellowAsset assets
      renderBonus (Bonus (Disque cx cy r) t _ _) = Translate cx cy $ case t of
        Health     -> healthBonusAsset assets
        Speed      -> speedBonusAsset assets
        Invincibility -> invBonusAsset assets

handleEvents :: Event -> GameControl -> GameControl
handleEvents ev (GameControl kbd gs) =
  case ev of

    EventKey (SpecialKey KeySpace) Down _ _ -> 
      (GameControl kbd (shoot gs))
    
    EventKey (Char 'r') Down _ _ ->
      GameControl initKeyboard initGameState

    -- default
    _ ->  GameControl (handleKeyEvent ev kbd) gs

update :: Float -> GameControl -> GameControl
update _ (GameControl kbd gs) = 
  if lost gs then ( GameControl kbd (updateScroll gs ) )
    else
    let gs1 = if isKeyDown (SpecialKey KeyLeft) kbd then moveLeft gs else gs in 
    let gs2 = if isKeyDown (SpecialKey KeyRight) kbd then moveRight gs1 else gs1 in 
    let gs3 = if isKeyDown (SpecialKey KeyUp) kbd then moveUp gs2 else gs2 in
    let gs4 = if isKeyDown (SpecialKey KeyDown) kbd then moveDown gs3 else gs3 in
    let gs5 = updateProjectiles gs4 in
    let gs6 = updateEnnemies gs5 in
    let gs7 = updateScroll gs6 in
    let gs8 = updateBonuses gs7 in
    let gs9 = updateSpeedTimer gs8 in
    
    --let gs6 = updateWalls gs5 in
      
    GameControl kbd gs9


--updtateScroll :: GameState -> GameState
--updateScroll gs@(GameState _ _ _ _ _ sc) = gs { scroll = sc + scrollSpeed }



main :: IO ()
main = do
  assets <- Assets
    <$> loadBMP "./assets/background.bmp"
    <*> loadBMP "./assets/perso.bmp"
    <*> loadBMP "./assets/perso_inv.bmp"
    <*> loadBMP "./assets/perso_speed2.bmp"
    <*> loadBMP "./assets/ball.bmp"
    <*> loadBMP "./assets/tear.bmp"
    <*> loadBMP "./assets/redE.bmp"
    <*> loadBMP "./assets/greenE.bmp"
    <*> loadBMP "./assets/blueE.bmp"
    <*> loadBMP "./assets/yellowE.bmp"
    <*> mapM (\n -> loadBMP $ "./assets/" <> show n <> "life.bmp") [0..5]
    <*> loadBMP "./assets/gameover.bmp"
    <*> loadBMP "./assets/bonus_health.bmp"
    <*> loadBMP "./assets/bonus_speed.bmp"
    <*> loadBMP "./assets/bonus_invincibility.bmp"

  play (InWindow "Minijeu" (566, 358) (10, 10))
       black 60
       initGame
       (render assets)
       handleEvents
       update


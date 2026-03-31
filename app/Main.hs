module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Debug.Trace (trace)

import Model

import Keyboard (Keyboard, initKeyboard, handleKeyEvent, isKeyDown, isInside)

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

render :: Picture -> Picture -> Picture -> Picture -> GameControl -> Picture
render bgnd perso projectile ennemy (GameControl _ (GameState player projs enns _)) =
  -- trace ("px=" <> (show (persoX player)) <> ", py=" <> (show (persoY player)))
  Pictures [bgnd, (Translate (persoX player) (persoY player) perso), Pictures (map renderProjectile projs), Pictures (map renderEnnemy enns)]
  where
    renderProjectile (Projectile _ _ (Disque cx cy r) dir) = Translate cx cy projectile
    renderEnnemy (Ennemy _ (Disque cx cy r) _) = Translate cx cy ennemy


handleEvents :: Event -> GameControl -> GameControl
handleEvents ev (GameControl kbd gs) =
  case ev of

    EventKey (SpecialKey KeySpace) Down _ _ -> 
      (GameControl kbd (shoot gs))
    
    -- Mouse click (your existing code)
    EventKey (MouseButton LeftButton) Down _ (mx, my) ->
      let GameState player _ _ _= gs
          touched = isInside mx my (persoX player) (persoY player)
      in if touched
        then trace "Touché !" (GameControl (handleKeyEvent ev kbd) gs)
         else GameControl kbd gs

    -- default
    _ ->  GameControl (handleKeyEvent ev kbd) gs

update :: Float -> GameControl -> GameControl
update _ (GameControl kbd gs) = 
  let gs1 = if isKeyDown (SpecialKey KeyLeft) kbd then moveLeft gs else gs in 
  let gs2 = if isKeyDown (SpecialKey KeyRight) kbd then moveRight gs1 else gs1 in 
  let gs3 = if isKeyDown (SpecialKey KeyUp) kbd then moveUp gs2 else gs2 in
  let gs4 = if isKeyDown (SpecialKey KeyDown) kbd then moveDown gs3 else gs3 in
  let gs5 = updateProjectiles gs4 in
  let gs6 = updateEnnemies gs5 in
  --let gs5 = updateScroll gs4 in
  --let gs6 = updateWalls gs5 in
    
  GameControl kbd gs6


--updtateScroll :: GameState -> GameState
--updateScroll gs@(GameState _ _ _ _ _ sc) = gs { scroll = sc + scrollSpeed }



main :: IO ()
main = do
  bgnd <- loadBMP "./assets/background.bmp"
  perso <- loadBMP "./assets/perso.bmp"
  projectile <- loadBMP "./assets/ball.bmp"
  ennemy <- loadBMP "./assets/ennemy.bmp"
  
  play (InWindow "Minijeu" (640, 360) (10, 10)) 
       black
        60
        initGame
        (render bgnd perso projectile ennemy)
        handleEvents
        update


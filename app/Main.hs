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

render :: Picture -> Picture -> Picture ->  Picture -> Picture -> Picture -> Picture -> Picture -> GameControl -> Picture
render bgnd perso ball ennemyRed ennemyGreen ennemyBlue ennemyYellow tear (GameControl _ (GameState (Player _ (Model.Rectangle px py pw ph) _) projs enns est sc)) =
  -- trace ("px=" <> (show (persoX player)) <> ", py=" <> (show (persoY player)))
  let bg = Pictures [ Translate 0 sc bgnd   
            , Translate 0 (sc+358) bgnd ] in
  Pictures [bg, (Translate (px+ pw/2) (py+ph/2) perso), Pictures (map renderProjectile projs), Pictures (map renderEnnemy enns)]
  where
    renderProjectile (Projectile _ (Disque cx cy r) dir t) = Translate cx cy (case t of
                                              Bullet -> ball
                                              Tear -> tear)
    renderEnnemy (Ennemy _ (Disque cx cy r) _ t _ _) = Translate cx cy (case t of
                                              Red -> ennemyRed
                                              Green -> ennemyGreen
                                              Blue -> ennemyBlue
                                              Yellow -> ennemyYellow)


handleEvents :: Event -> GameControl -> GameControl
handleEvents ev (GameControl kbd gs) =
  case ev of

    EventKey (SpecialKey KeySpace) Down _ _ -> 
      (GameControl kbd (shoot gs))
    

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
  let gs7 = updateScroll gs6 in
  --let gs5 = updateScroll gs4 in
  --let gs6 = updateWalls gs5 in
    
  GameControl kbd gs7


--updtateScroll :: GameState -> GameState
--updateScroll gs@(GameState _ _ _ _ _ sc) = gs { scroll = sc + scrollSpeed }



main :: IO ()
main = do
  bgnd <- loadBMP "./assets/background.bmp"
  perso <- loadBMP "./assets/perso.bmp"
  ball <- loadBMP "./assets/ball.bmp"
  ennemyRed <- loadBMP "./assets/redE.bmp"
  ennemyGreen <- loadBMP "./assets/greenE.bmp"
  ennemyBlue <- loadBMP "./assets/blueE.bmp"
  ennemyYellow <- loadBMP "./assets/yellowE.bmp"
  tear <- loadBMP "./assets/tear.bmp"
  
  play (InWindow "Minijeu" (566, 358) (10, 10)) 
       black
        60
        initGame
        (render bgnd perso ball ennemyRed ennemyGreen ennemyBlue ennemyYellow tear)
        handleEvents
        update


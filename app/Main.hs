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

render :: Picture -> Picture -> GameControl -> Picture
render bgnd perso (GameControl _ (GameState px py _)) =
  -- trace ("px=" <> (show px) <> ", py=" <> (show py))
  Pictures [bgnd, (Translate px py perso)]


handleEvents :: Event -> GameControl -> GameControl
handleEvents ev (GameControl kbd gs) =
  case ev of

    -- Mouse click (your existing code)
    EventKey (MouseButton LeftButton) Down _ (mx, my) ->
      let GameState px py _ = gs
          touched = isInside mx my px py
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
  
    
  GameControl kbd gs4



main :: IO ()
main = do
  bgnd <- loadBMP "./assets/background.bmp"
  perso <- loadBMP "./assets/perso.bmp"
  
  play (InWindow "Minijeu" (640, 360) (10, 10)) 
       black
        60
        initGame
        (render bgnd perso)
        handleEvents
        update


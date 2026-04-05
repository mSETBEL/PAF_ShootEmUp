module Keyboard where

import Graphics.Gloss.Interface.IO.Interact

import Data.Set (Set)
import qualified Data.Set as S

-- import Debug.Trace (trace)

type Keyboard = Set Key

-- | création de la structure d'état de clavier (vide)
initKeyboard :: Keyboard
initKeyboard = S.empty

handleKeyEvent :: Event -> Keyboard -> Keyboard
handleKeyEvent (EventKey key Down _ _) kbd = 
    --trace ("Key down: " <> (show key))
    S.insert key kbd
handleKeyEvent (EventKey key Up _ _) kbd = 
    --trace ("Key up: " <> (show key))
    S.delete key kbd
handleKeyEvent _ kbd = kbd   

isKeyDown :: Key -> Keyboard -> Bool
isKeyDown key kbd = S.member key kbd

  
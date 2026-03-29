module Model where


data GameState = GameState { persoX :: Float
                           , persoY :: Float
                           , speed :: Float }
  deriving (Show)


initGameState :: GameState
initGameState = GameState 0 0 2

moveLeft :: GameState -> GameState
moveLeft gs@(GameState px _ sp) | px > -310 = gs { persoX = px - sp }
                                | otherwise = gs

moveRight :: GameState -> GameState
moveRight gs@(GameState px _ sp) | px < 310 = gs { persoX = px + sp }
                                 | otherwise = gs
                              
moveDown :: GameState -> GameState
moveDown gs@(GameState _ py sp) | py > -160  = gs { persoY = py - sp }
                              | otherwise = gs

moveUp :: GameState -> GameState
moveUp gs@(GameState _ py sp) | py < 160 = gs { persoY = py + sp }
                                | otherwise = gs

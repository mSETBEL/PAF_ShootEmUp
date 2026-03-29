module Model where


data GameState = GameState { persoX :: Float
                           , persoY :: Float
                           , speed :: Float 
                           , murGauche :: MurGauche
                           , murDroit :: MurDroit
                           , scroll :: Float
                           }
  deriving (Show)


data Hitbox = Point Int Int
            | Disque Int Int Int       -- centre x, centre y, rayon
            | Rectangle Int Int Int Int -- x, y, largeur, hauteur
            | Composee [Hitbox]
            | MurGauche [(Int, Int)]
            | MurDroit [(Int, Int)]
            deriving (Eq, Show)



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



collision :: Hitbox -> Hitbox -> Bool
collision (Point x1 y1) (Point x2 y2) = 
  x1 == x2 && y1 == y2

collision (Point x y) (Disque cx cy r) = 
  (x-cx)^2 + (y-cy)^2 <= r^2

collision (Point x y) (Rectangle rx ry w h) =
  x >= rx && x <= rx+w && y >= ry && y <= ry+h

collision (Disque x1 y1 r1) (Disque x2 y2 r2) =
  (x1-x2)^2 + (y1-y2)^2 <= (r1+r2)^2

collision (Disque cx cy r) (Rectangle rx ry w h) =
  collision (Rectangle (cx-r) (cy-r) (2*r) (2*r)) (Rectangle rx ry w h)

collision (Rectangle x1 y1 w1 h1) (Rectangle x2 y2 w2 h2) =
  x1 < x2+w2 && x1+w1 > x2 && y1 < y2+h2 && y1+h1 > y2

collision (Composee hs) other = 
  any (`collision` other) hs

collision (MurGauche segs) (Point px py) =
  case findSegment py segs of
    Nothing          -> False
    Just (x1,y1,x2,y2) -> 
      let t     = fromIntegral (py - y1) / fromIntegral (y2 - y1) :: Float
          wallX = fromIntegral x1 + t * fromIntegral (x2 - x1)
      in fromIntegral px <= wallX

collision (MurDroit segs) (Point px py) =
  case findSegment py segs of
    Nothing          -> False
    Just (x1,y1,x2,y2) -> 
      let t     = fromIntegral (py - y1) / fromIntegral (y2 - y1) :: Float
          wallX = fromIntegral x1 + t * fromIntegral (x2 - x1)
      in fromIntegral px >= wallX




findSegment :: Int -> [(Int,Int)] -> Maybe (Int,Int,Int,Int)
findSegment py pts = go pts
  where
    go ((x1,y1):(x2,y2):rest)
      | py >= y1 && py < y2 = Just (x1,y1,x2,y2)
      | otherwise            = go ((x2,y2):rest)
    go _ = Nothing
collision a b = collision b a
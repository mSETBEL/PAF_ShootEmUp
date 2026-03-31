module Model where


data GameState = GameState { player :: Player
                           , projectiles :: [Projectile]
                           , enemies :: [Ennemy]
                           , ennemySpawnTimer :: Int
                           , scrollOffset :: Float
                           }
  deriving (Show)

data Direction = LeftDir | RightDir | UpDir | DownDir
  deriving (Show, Eq)

data Player = Player {
                      persoX :: Float
                    , persoY :: Float
                    , persoSpeed :: Float
                    }
  deriving (Show)
data Projectile = Projectile {
                             projSpeed :: Int
                           , projTick :: Int
                           , projHitbox :: Hitbox
                           , projDirection :: Direction 
                           }
  deriving (Show)

data Ennemy = Ennemy {  
                          ennemySpeed :: Float
                          , ennemyHitbox :: Hitbox
                          , ennemyDirection :: Direction
                          }
  deriving (Show)

ennemySpawnSpeed :: Int
ennemySpawnSpeed = 100

data Hitbox = Point Float Float
            | Disque Float Float Float       -- centre x, centre y, rayon
            | Rectangle Float Float Float Float -- x, y, largeur, hauteur
            | Composee [Hitbox]
            | MurGauche [(Float, Float)]
            | MurDroit [(Float, Float)]
            deriving (Eq, Show)

scrollSpeed :: Float
scrollSpeed = 0.5

initPlayer :: Player
initPlayer = Player 0 0 2

initGameState :: GameState
initGameState = GameState initPlayer [] [] ennemySpawnSpeed 0.0

moveLeft :: GameState -> GameState
moveLeft gs@(GameState player _ _ _ _) | persoX player > -273 = gs { player = player { persoX = persoX player - persoSpeed player } }
                                | otherwise = gs

moveRight :: GameState -> GameState
moveRight gs@(GameState player _ _ _ _) | persoX player < 273 = gs { player = player { persoX = persoX player + persoSpeed player } }
                                 | otherwise = gs
                              
moveDown :: GameState -> GameState
moveDown gs@(GameState player _ _ _ _) | persoY player > -159  = gs { player = player { persoY = persoY player - persoSpeed player } }
                              | otherwise = gs

moveUp :: GameState -> GameState
moveUp gs@(GameState player _ _ _ _) | persoY player < 159 = gs { player = player { persoY = persoY player + persoSpeed player } }
                                | otherwise = gs


-- Projectile stuff
shoot :: GameState -> GameState
shoot gs@(GameState player projs _ _ _) =
  let newProj = Projectile 1 1 (Disque (persoX player) (persoY player + 20) 4) UpDir
  in gs { projectiles = newProj : projs }

tickProjectile :: Projectile -> Projectile
tickProjectile proj@(Projectile sp tick hitbox dir) =
  if tick > 0
    then proj { projTick = tick - 1 }
    else moveProjectile proj

moveProjectile :: Projectile -> Projectile
moveProjectile proj@(Projectile sp tick (Disque cx cy r) dir) =
  let (dx, dy) = case dir of
                    LeftDir  -> (-1, 0)
                    RightDir -> (1, 0)
                    UpDir    -> (0, 1)
                    DownDir  -> (0, -1)
      newCx = cx + dx 
      newCy = cy + dy 
  in proj { projHitbox = Disque newCx newCy r, projTick = sp }

cullProjectile :: [Projectile] -> [Projectile]
cullProjectile = filter (\p -> onScreen p) 
  where
    onScreen (Projectile _ _ (Disque cx cy r) _) =
      cx + r >= -283 && cx - r <= 283 && cy + r >= -179 && cy - r <= 179

updateProjectiles :: GameState -> GameState
updateProjectiles (GameState player projs e est scrollOffset) =
  let updatedProjs = map tickProjectile projs
      culledProjs = cullProjectile updatedProjs
  in GameState player culledProjs e est scrollOffset


-- enemy stuff
moveEnnemy :: Ennemy -> Ennemy
moveEnnemy ennemy@(Ennemy speed (Disque cx cy r) dir) =

  let newDir = case cx of
                  x | x <= -275 -> RightDir
                    | x >= 275  -> LeftDir
                    | otherwise -> dir
  in let (newCx, newCy) = case newDir of
                    LeftDir  -> (cx-speed, cy)
                    RightDir -> (cx+speed, cy)
                    UpDir    -> (cx, cy+speed)
                    DownDir  -> (cx, cy-speed)
  in ennemy { ennemyHitbox = Disque newCx newCy r, ennemyDirection = newDir }

spawnEnnemy :: GameState -> GameState
spawnEnnemy gs@(GameState player projs enns est _) =
  let newEnnemy = Ennemy 2 (Disque (-275) 130 8) RightDir
  in gs { enemies = newEnnemy : enns }

killEnnemy :: GameState -> GameState
killEnnemy gs@(GameState player projs enns est _) =
  let updatedEnns = filter (not . isKilled) enns
  in gs { enemies = updatedEnns }
  where
    isKilled ennemy = any (\proj -> collision (projHitbox proj) (ennemyHitbox ennemy)) projs

updateEnnemies :: GameState -> GameState
updateEnnemies gs@(GameState player projs enns est _) =
  let moved  = gs { enemies = map moveEnnemy enns }
      killed = killEnnemy moved
  in if est <= 0
     then spawnEnnemy (killed { ennemySpawnTimer = ennemySpawnSpeed })
     else killed { ennemySpawnTimer = est - 1 }
--
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
      let t     = (py - y1) /  (y2 - y1) :: Float
          wallX =  x1 + t * (x2 - x1)
      in  px <= wallX

collision (MurDroit segs) (Point px py) =
  case findSegment py segs of
    Nothing          -> False
    Just (x1,y1,x2,y2) -> 
      let t     = (py - y1) /  (y2 - y1) :: Float
          wallX =  x1 + t * (x2 - x1)
      in  px >= wallX
collision a b = collision b a



findSegment :: Float -> [(Float,Float)] -> Maybe (Float,Float,Float,Float)
findSegment py pts = go pts
  where
    go ((x1,y1):(x2,y2):rest)
      | py >= y1 && py < y2 = Just (x1,y1,x2,y2)
      | otherwise            = go ((x2,y2):rest)
    go _ = Nothing

--scroll stuff
updateScroll :: GameState -> GameState
updateScroll gs = gs { scrollOffset = if scrollOffset gs <= -358 then 0 
                                 else scrollOffset gs - scrollSpeed }
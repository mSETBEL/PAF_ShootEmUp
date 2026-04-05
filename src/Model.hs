module Model where


-- Types de données pour le jeu
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
                      persoSpeed :: Float
                    , persoHitbox :: Hitbox
                    , persoHealth :: Int
                    }
  deriving (Show)
data Projectile = Projectile {
                             projSpeed :: Float
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


data Hitbox = Point Float Float
            | Disque Float Float Float       -- centre x, centre y, rayon
            | Rectangle Float Float Float Float -- x, y, largeur, hauteur
            | Composee [Hitbox]
            | MurGauche [(Float, Float)]
            | MurDroit [(Float, Float)]
            deriving (Eq, Show)


-- constantes de jeu
ennemySpawnSpeed :: Int
ennemySpawnSpeed = 100
scrollSpeed :: Float
scrollSpeed = 0.5

screenWidth :: Float
screenWidth = 566 
screenHeight :: Float
screenHeight = 358

projectileCOte :: Float
projectileCOte = 16

ennemyCote :: Float
ennemyCote = 30

playerHeight :: Float
playerHeight = 72
playerWidth :: Float
playerWidth = 42

--constructeurs 
initPlayer :: Player
initPlayer = Player 3 (Rectangle 0 (-100) playerWidth playerHeight) 5

initEnnemy :: Ennemy
initEnnemy = Ennemy 2 (Disque (-275) 130 (ennemyCote / 2)) RightDir

initProjectile :: Float -> Float -> Projectile
initProjectile x y = Projectile 2 (Disque x y (projectileCOte / 2)) UpDir

initGameState :: GameState
initGameState = GameState initPlayer [] [] ennemySpawnSpeed 0.0



moveLeft :: GameState -> GameState
moveLeft gs@(GameState player _ _ _ _) =
  case persoHitbox player of
    Rectangle x y w h | x > -(screenWidth / 2 - w / 2) ->
      gs { player = player { persoHitbox = Rectangle (x - persoSpeed player) y w h } }
    _ -> gs

moveRight :: GameState -> GameState
moveRight gs@(GameState player _ _ _ _) =
  case persoHitbox player of
    Rectangle x y w h | x < screenWidth / 2 - w / 2 ->
      gs { player = player { persoHitbox = Rectangle (x + persoSpeed player) y w h } }
    _ -> gs

moveUp :: GameState -> GameState
moveUp gs@(GameState player _ _ _ _) =
  case persoHitbox player of
    Rectangle x y w h | y < screenHeight / 2 - h / 2 ->
      gs { player = player { persoHitbox = Rectangle x (y + persoSpeed player) w h } }
    _ -> gs

moveDown :: GameState -> GameState
moveDown gs@(GameState player _ _ _ _) =
  case persoHitbox player of
    Rectangle x y w h | y > -(screenHeight / 2 - h / 2) ->
      gs { player = player { persoHitbox = Rectangle x (y - persoSpeed player) w h } }
    _ -> gs

-- Projectile stuff
shoot :: GameState -> GameState
shoot gs@(GameState (Player _ (Rectangle px py pw ph) _) projs _ _ _) =
  let newProj =  initProjectile px (py + 25)
  in gs { projectiles = newProj : projs }


moveProjectile :: Projectile -> Projectile
moveProjectile proj@(Projectile sp (Disque cx cy r) dir) =
  let (dx, dy) = case dir of
                    LeftDir  -> (-sp, 0)
                    RightDir -> (sp, 0)
                    UpDir    -> (0, sp)
                    DownDir  -> (0, -sp)
      newCx = cx + dx 
      newCy = cy + dy 
  in proj { projHitbox = Disque newCx newCy r}

cullProjectile :: [Projectile] -> [Projectile] -- Supprime les projectiles qui sont hors de l'écran
cullProjectile = filter (\p -> onScreen p) 
  where
    onScreen (Projectile _ (Disque cx cy r) _) =
      cx + r >= -screenWidth / 2 && cx - r <= screenWidth / 2 && cy + r >= -screenHeight / 2 && cy - r <= screenHeight / 2


updateProjectiles :: GameState -> GameState
updateProjectiles (GameState player projs e est scrollOffset) =
  let updatedProjs = map moveProjectile projs
      culledProjs = cullProjectile updatedProjs
  in GameState player culledProjs e est scrollOffset


-- enemy stuff
moveEnnemy :: Ennemy -> Ennemy
moveEnnemy ennemy@(Ennemy speed (Disque cx cy r) dir) =

  let newDir = case cx of
                  x | x <= -(screenWidth / 2 - ennemyCote / 2) -> RightDir
                    | x >= screenWidth / 2 - ennemyCote / 2  -> LeftDir
                    | otherwise -> dir
  in let (newCx, newCy) = case newDir of
                    LeftDir  -> (cx-speed, cy)
                    RightDir -> (cx+speed, cy)
                    UpDir    -> (cx, cy+speed)
                    DownDir  -> (cx, cy-speed)
  in ennemy { ennemyHitbox = Disque newCx newCy r, ennemyDirection = newDir }

spawnEnnemy :: GameState -> GameState
spawnEnnemy gs@(GameState player projs enns est _) =
  let newEnnemy = initEnnemy
  in gs { enemies = newEnnemy : enns }

killEnnemy :: GameState -> GameState
killEnnemy gs@(GameState player projs enns est _) =
  let updatedEnns = filter (not . isKilled) enns in
  let updatedProj = filter (not . hasKilled) projs
  in gs { enemies = updatedEnns, projectiles = updatedProj }
  where
    isKilled ennemy = any (\proj -> collision (projHitbox proj) (ennemyHitbox ennemy)) projs
    hasKilled proj = any (\ennemy -> collision (projHitbox proj) (ennemyHitbox ennemy)) enns


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
updateScroll gs = gs { scrollOffset = if scrollOffset gs <= -screenHeight then 0 
                                 else scrollOffset gs - scrollSpeed }






-- preconditions movements
prop_pre_moveLeft :: GameState -> Bool
prop_pre_moveLeft (GameState (Player _ (Rectangle x _ w _) _) _ _ _ _) =
  x > -(screenWidth / 2 - w / 2)
prop_pre_moveLeft _ = False

prop_pre_moveRight :: GameState -> Bool
prop_pre_moveRight (GameState (Player _ (Rectangle x _ w _) _) _ _ _ _) =
  x < screenWidth / 2 - w / 2
prop_pre_moveRight _ = False

prop_pre_moveUp :: GameState -> Bool
prop_pre_moveUp (GameState (Player _ (Rectangle _ y _ h) _) _ _ _ _) =
  y < screenHeight / 2 - h / 2
prop_pre_moveUp _ = False

prop_pre_moveDown :: GameState -> Bool
prop_pre_moveDown (GameState (Player _ (Rectangle _ y _ h) _) _ _ _ _) =
  y > -(screenHeight / 2 - h / 2)
prop_pre_moveDown _ = False


-- preconditions shoot
prop_pre_shoot :: GameState -> Bool
prop_pre_shoot (GameState (Player _ (Rectangle _ _ _ _) _) _ _ _ _) = True
prop_pre_shoot _ = False

-- preconditions spawn timer
prop_pre_spawnTimer :: GameState -> Bool
prop_pre_spawnTimer gs = ennemySpawnTimer gs <= 0

-- preconditions update scroll
prop_pre_updateScroll :: GameState -> Bool
prop_pre_updateScroll gs = scrollOffset gs > -screenHeight && scrollOffset gs <= 0



-- invariants

-- le joueur doit être dans les limites de l'écran
prop_inv_player :: GameState -> Bool
prop_inv_player (GameState (Player sp (Rectangle px py pw ph) hp) _ _ _ _) =
  sp > 0 
  && px >= -(screenWidth / 2 - pw / 2) && px <= screenWidth / 2 - pw / 2
  && py >= -(screenHeight / 2 - ph / 2) && py <= screenHeight / 2 - ph / 2
  && hp >= 0
prop_inv_player _ = False

-- les projectiles doivent être dans les limites de l'écran
prop_inv_projectiles :: GameState -> Bool
prop_inv_projectiles (GameState _ projs _ _ _) =
  all valid projs
  where
    valid (Projectile sp (Disque cx cy r) _) =
      sp > 0
      && cx + r >= -(screenWidth / 2) && cx - r <= screenWidth / 2
      && cy + r >= -(screenHeight / 2) && cy - r <= screenHeight / 2
    valid _ = False

-- les ennemis doivent être dans les limites de l'écran
prop_inv_enemies :: GameState -> Bool
prop_inv_enemies (GameState _ _ enns _ _) =
  all valid enns
  where
    valid (Ennemy sp (Disque cx cy r) _) =
      sp > 0
      && cx + r >= -(screenWidth / 2) && cx - r <= screenWidth / 2
      && cy + r >= -(screenHeight / 2) && cy - r <= screenHeight / 2
    valid _ = False


-- le scroll doit être entre le bas de l'écran et le haut de l'écran
prop_inv_scroll :: GameState -> Bool
prop_inv_scroll gs = scrollOffset gs > -screenHeight && scrollOffset gs <= 0

-- spawn timer doit être positif
prop_inv_spawnTimer :: GameState -> Bool
prop_inv_spawnTimer gs = ennemySpawnTimer gs >= 0

-- invariant global
prop_inv_GameState :: GameState -> Bool
prop_inv_GameState gs =
  prop_inv_player gs
  && prop_inv_projectiles gs
  && prop_inv_enemies gs
  && prop_inv_scroll gs
  && prop_inv_spawnTimer gs


--postconditions

prop_post_moveLeft :: GameState -> Bool
prop_post_moveLeft gs@(GameState (Player sp (Rectangle x _ _ _) _) _ _ _ _) =
  case moveLeft gs of
    GameState (Player _ (Rectangle x2 _ _ _) _) _ _ _ _ -> 
      if prop_pre_moveLeft gs
        then x2 == x - sp
        else x2 == x
    _ -> False
    

prop_post_moveRight :: GameState -> Bool
prop_post_moveRight gs@(GameState (Player sp (Rectangle x _ _ _) _) _ _ _ _) =
  case moveRight gs of
    GameState (Player _ (Rectangle x2 _ _ _) _) _ _ _ _ ->
      if prop_pre_moveRight gs
        then x2 == x + sp
        else x2 == x
    _ -> False

prop_post_moveUp :: GameState -> Bool
prop_post_moveUp gs@(GameState (Player sp (Rectangle _ y _ _) _) _ _ _ _) =
  case moveUp gs of
    GameState (Player _ (Rectangle _ y2 _ _) _) _ _ _ _ ->
      if prop_pre_moveUp gs
        then y2 == y + sp
        else y2 == y
    _ -> False

prop_post_moveDown :: GameState -> Bool
prop_post_moveDown gs@(GameState (Player sp (Rectangle _ y _ _) _) _ _ _ _) =
  case moveDown gs of
    GameState (Player _ (Rectangle _ y2 _ _) _) _ _ _ _ ->
      if prop_pre_moveDown gs
        then y2 == y - sp
        else y2 == y
    _ -> False

-- postconditions shoot (le nombre de projectiles doit augmenter de 1)
prop_post_shoot :: GameState -> Bool
prop_post_shoot gs = length (projectiles (shoot gs)) == length (projectiles gs) + 1

-- postconditions spawn ennemy (le nombre d'ennemis doit augmenter de 1)
prop_post_spawnEnnemy :: GameState -> Bool
prop_post_spawnEnnemy gs = length (enemies (spawnEnnemy gs)) == length (enemies gs) + 1
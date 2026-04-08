module Model where


-- Types de données pour le jeu
data GameState = GameState { player :: Player
                           , projectiles :: [Projectile]
                           , enemies :: [Ennemy]
                           , ennemySpawnTimer :: Int
                           , scrollOffset :: Float
                           }
  deriving (Show)

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
                           , projType :: ProjType
                           }
  deriving (Show)

data Ennemy = Ennemy {  
                          ennemySpeed :: Float
                          , ennemyHitbox :: Hitbox
                          , ennemyDirection :: (Float, Float)
                          , ennemyType :: EnemyType
                          , ennemyPhase :: Float
                          , onScreen :: Bool
                          }
  deriving (Show)

data Direction = LeftDir | RightDir | UpDir | DownDir
  deriving (Show, Eq)
data EnemyType = Red | Green | Blue | Yellow 
  deriving (Show, Eq)
data ProjType = Bullet | Tear
  deriving (Show, Eq)

data Hitbox = Point Float Float
            | Disque Float Float Float       -- centre x, centre y, rayon
            | Rectangle Float Float Float Float -- x, y, largeur, hauteur
            | Composee [Hitbox]
            | MurGauche [(Float, Float)]
            | MurDroit [(Float, Float)]
            deriving (Eq, Show)


-- constantes de jeu
ennemySpawnSpeed :: Int
ennemySpawnSpeed = 900
scrollSpeed :: Float
scrollSpeed = 0.5

screenWidth :: Float
screenWidth = 566 
screenHeight :: Float
screenHeight = 358

projectileCOte :: Float
projectileCOte = 8

ennemyCote :: Float
ennemyCote = 30

playerHeight :: Float
playerHeight = 72
playerWidth :: Float
playerWidth = 42

tearTimer :: Float
tearTimer = 40

--constructeurs 
initPlayer :: Player
initPlayer = Player 2 (Rectangle (-playerWidth / 2) (-100) playerWidth playerHeight) 5


initProjectile :: Float -> Float -> Direction  -> ProjType -> Projectile
initProjectile x y dir t = Projectile 5 (Disque x y (projectileCOte / 2)) dir t

initGameState :: GameState
initGameState = GameState initPlayer [] [] 50 0.0



moveUp :: GameState -> GameState
moveUp gs@(GameState player _ _ _ _) =
  case persoHitbox player of
    Rectangle x y w h ->
      let newY = min (screenHeight / 2 - h ) (y + persoSpeed player)
      in gs { player = player { persoHitbox = Rectangle x newY w h } }
    _ -> gs

moveDown :: GameState -> GameState
moveDown gs@(GameState player _ _ _ _) =
  case persoHitbox player of
    Rectangle x y w h ->
      let newY = max (-(screenHeight / 2 )) (y - persoSpeed player)
      in gs { player = player { persoHitbox = Rectangle x newY w h } }
    _ -> gs

moveLeft :: GameState -> GameState
moveLeft gs@(GameState player _ _ _ _) =
  case persoHitbox player of
    Rectangle x y w h ->
      let newX = max (-(screenWidth / 2)) (x - persoSpeed player)
      in gs { player = player { persoHitbox = Rectangle newX y w h } }
    _ -> gs

moveRight :: GameState -> GameState
moveRight gs@(GameState player _ _ _ _) =
  case persoHitbox player of
    Rectangle x y w h ->
      let newX = min (screenWidth / 2 - w) (x + persoSpeed player)
      in gs { player = player { persoHitbox = Rectangle newX y w h } }
    _ -> gs

-- Projectile stuff
shoot :: GameState -> GameState
shoot gs@(GameState (Player _ (Rectangle px py pw ph) _) projs _ _ _) =
  let newProj =  initProjectile (px+pw/2) (py + ph) UpDir Bullet
  in gs { projectiles = newProj : projs }


moveProjectile :: Projectile -> Projectile
moveProjectile proj@(Projectile sp (Disque cx cy r) dir _) =
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
    onScreen (Projectile _ (Disque cx cy r) _ _) =
      cx + r >= -screenWidth / 2 && cx - r <= screenWidth / 2 && cy + r >= -screenHeight / 2 && cy - r <= screenHeight / 2


updateProjectiles :: GameState -> GameState
updateProjectiles (GameState player projs e est scrollOffset) =
  let updatedProjs = map moveProjectile projs
      culledProjs = cullProjectile updatedProjs
  in GameState player culledProjs e est scrollOffset


-- enemy stuff
moveEnnemy :: Ennemy -> (Ennemy, Maybe Projectile)
moveEnnemy ennemy = case ennemyType ennemy of
  Red    -> (moveRedEnnemy ennemy, Nothing)
  Yellow -> (moveYellowEnnemy ennemy, Nothing)
  Blue   -> moveBlueEnnemy ennemy
  _      -> (ennemy, Nothing)


moveRedEnnemy :: Ennemy -> Ennemy
moveRedEnnemy e@(Ennemy sp (Disque cx cy r) (dx, dy) Red phase _) =
  let
    newCx = cx + dx * sp
    (newDx, finalCx) =
      if (newCx + r >= screenWidth / 2 && dx > 0) ||
         (newCx - r <= -(screenWidth / 2) && dx < 0)
      then (-dx, cx + (-dx) * sp)
      else (dx, newCx)

    newPhase = phase + 0.05
    newCy = cy + sin newPhase * 2   

  in updateOnScreen( e { ennemyHitbox = Disque finalCx newCy r
       , ennemyDirection = (newDx, dy)
       , ennemyPhase = newPhase
   })

moveYellowEnnemy :: Ennemy -> Ennemy
moveYellowEnnemy e@(Ennemy sp (Disque cx cy r) (dx, dy) Yellow _ _ ) =
  let (newDx, finalCx) =
        if cx + r >= screenWidth / 2 && dx > 0
        then (-dx, screenWidth / 2 - r)
        else if cx - r <= -(screenWidth / 2) && dx < 0
        then (-dx, -(screenWidth / 2) + r)
        else (dx, cx + dx * sp)
      (newDy, finalCy) =
        if cy + r >= screenHeight / 2 && dy > 0
        then (-dy, screenHeight / 2 - r)
        else if cy - r <= -(screenHeight / 2) && dy < 0
        then (-dy, -(screenHeight / 2) + r)
        else (dy, cy + dy * sp)
  in updateOnScreen (e { ennemyHitbox    = Disque finalCx finalCy r
       , ennemyDirection = (newDx, newDy) })


moveBlueEnnemy :: Ennemy -> (Ennemy, Maybe(Projectile))
moveBlueEnnemy e@(Ennemy sp (Disque cx cy r) (dx, dy) Blue phase onScreen) =
  let newCx = cx + dx * sp
      (newDx, finalCx) =
        if newCx + r >= screenWidth / 2  && dx > 0
        then (-dx, screenWidth / 2 - r)
        else if newCx - r <= -(screenWidth / 2) && dx < 0  
        then (-dx, -(screenWidth / 2) + r)         
        else (dx, newCx)

      newPhase  = phase - 1
      shoots    = newPhase <= 0
      proj      = if shoots && onScreen
                  then Just (initProjectile finalCx (cy - r) DownDir Tear)
                  else Nothing
      finalPhase = if shoots then tearTimer else newPhase
  in ( updateOnScreen (e { ennemyHitbox     = Disque finalCx cy r
         , ennemyDirection  = (newDx, dy)
         , ennemyPhase      = finalPhase })
     , proj )


spawnYellowEnnemy :: Float -> Ennemy
spawnYellowEnnemy playerX = Ennemy 4
                               (Disque spawnX (-screenHeight / 2 - ennemyCote) (ennemyCote / 2))
                               (1, 1) Yellow 0 False
  where
    seed   = round playerX :: Int
    spawnX = fromIntegral ((seed * 1103515245 + 12345) `mod` round screenWidth) --arrive par un endroit selon la position du joueur
             - screenWidth / 2

spawnRedEnnemies :: [Ennemy]
spawnRedEnnemies = map makeOne [0..3]
  where
    r         = ennemyCote / 2
    gap       = ennemyCote + 5
    startX    = -(screenWidth / 2) - 4 * gap 
    makeOne i = Ennemy 2
                  (Disque (startX + fromIntegral i * gap) 50 r)
                  (1,0) Red 
                  (fromIntegral i * pi / 2) False

spawnBlueEnnemies :: [Ennemy]
spawnBlueEnnemies = 
                  [ Ennemy 2
                    (Disque (-(screenWidth / 2)) 100 (ennemyCote / 2))
                    (1, 0) Blue tearTimer False , 
                  Ennemy 2
                    (Disque ((screenWidth / 2)) 100 (ennemyCote / 2))
                    (-1, 0) Blue tearTimer  False
                  ]       


killEnnemy :: GameState -> GameState
killEnnemy gs@(GameState player projs enns est _) =
  let updatedEnns = filter (not . isKilled) enns in
  let updatedProj = filter (not . hasKilled) projs
  in gs { enemies = updatedEnns, projectiles = updatedProj }
  where
    isKilled ennemy = any (\proj ->  projType proj == Bullet && collision (projHitbox proj) (ennemyHitbox ennemy)) projs || collision (ennemyHitbox ennemy) (persoHitbox player)
    hasKilled proj = any (\ennemy -> projType proj == Bullet && collision (projHitbox proj) (ennemyHitbox ennemy)) enns --enlever le projectile qui a tué un ennemi

updateOnScreen :: Ennemy -> Ennemy
updateOnScreen e@(Ennemy _ (Disque cx cy r) _ _ _ onS) =
  e { onScreen = onS ||  -- quand l'ennemi rentre dans l'écran, il reste dans l'écran
        cx - r >= -(screenWidth / 2)
     && cx + r <= screenWidth / 2
     && cy - r >= -(screenHeight / 2)
     && cy + r <= screenHeight / 2 }

updateEnnemies :: GameState -> GameState
updateEnnemies gs@(GameState player projs enns est _) =
   
  let (movedEnns, newProjs) = unzip (map moveEnnemy enns)
      extraProjs             = [ p | Just p <- newProjs ]
      moved                  = gs { enemies      = movedEnns
                                  , projectiles  = projs ++ extraProjs }
      killed                 = killEnnemy moved
 
  in case est of
    0 -> killed { enemies = spawnRedEnnemies ++ enemies killed, ennemySpawnTimer = ennemySpawnSpeed }
    600 -> let px = case persoHitbox player of
                   Rectangle x _ _ _ -> x
                   _                 -> 0
        in killed { enemies          = spawnYellowEnnemy px : enemies killed
                  , ennemySpawnTimer = est - 1 }
    300 -> killed { enemies = spawnBlueEnnemies ++ enemies killed, ennemySpawnTimer = est-1 }
    
    _ -> killed { ennemySpawnTimer = est-1 }


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
  x > -(screenWidth / 2 )
prop_pre_moveLeft _ = False

prop_pre_moveRight :: GameState -> Bool
prop_pre_moveRight (GameState (Player _ (Rectangle x _ w _) _) _ _ _ _) =
  x < screenWidth / 2 - w
prop_pre_moveRight _ = False

prop_pre_moveUp :: GameState -> Bool
prop_pre_moveUp (GameState (Player _ (Rectangle _ y _ h) _) _ _ _ _) =
  y < screenHeight / 2 - h 
prop_pre_moveUp _ = False

prop_pre_moveDown :: GameState -> Bool
prop_pre_moveDown (GameState (Player _ (Rectangle _ y _ h) _) _ _ _ _) =
  y > -(screenHeight / 2 )
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
    valid (Projectile sp (Disque cx cy r) _ _) =
      sp > 0
      && cx + r >= -(screenWidth / 2) && cx - r <= screenWidth / 2
      && cy + r >= -(screenHeight / 2) && cy - r <= screenHeight / 2
    valid _ = False

-- les ennemis doivent être dans les limites de l'écran
prop_inv_enemies :: GameState -> Bool
prop_inv_enemies (GameState _ _ enns _ _) =
  all valid enns
  where
    valid (Ennemy sp (Disque cx cy r) _ _ _ _) =
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

prop_post_moveUp :: GameState -> Bool
prop_post_moveUp gs@(GameState (Player sp (Rectangle x y w h) _) _ _ _ _) =
  case moveUp gs of
    GameState (Player _ (Rectangle _ y2 _ _) _) _ _ _ _ ->
      if prop_pre_moveUp gs
      then y2 == min (screenHeight / 2 - h) (y + sp)  -- clamped
      else y2 == y
    _ -> False
prop_post_moveUp _ = False

prop_post_moveDown :: GameState -> Bool
prop_post_moveDown gs@(GameState (Player sp (Rectangle x y w h) _) _ _ _ _) =
  case moveDown gs of
    GameState (Player _ (Rectangle _ y2 _ _) _) _ _ _ _ ->
      if prop_pre_moveDown gs
      then y2 == max (-(screenHeight / 2 )) (y - sp)  -- clamped
      else y2 == y
    _ -> False
prop_post_moveDown _ = False


prop_post_moveLeft :: GameState -> Bool
prop_post_moveLeft gs@(GameState (Player sp (Rectangle x y w h) _) _ _ _ _) =
  case moveLeft gs of
    GameState (Player _ (Rectangle x2 _ _ _) _) _ _ _ _ ->
      if prop_pre_moveLeft gs
      then x2 == max (-(screenWidth / 2 )) (x - sp)
      else x2 == x
    _ -> False
prop_post_moveLeft _ = False

prop_post_moveRight :: GameState -> Bool
prop_post_moveRight gs@(GameState (Player sp (Rectangle x y w h) _) _ _ _ _) =
  case moveRight gs of
    GameState (Player _ (Rectangle x2 _ _ _) _) _ _ _ _ ->
      if prop_pre_moveRight gs
      then x2 == min (screenWidth / 2 - w ) (x + sp)
      else x2 == x
    _ -> False
prop_post_moveRight _ = False


-- postconditions shoot (le nombre de projectiles doit augmenter de 1)
prop_post_shoot :: GameState -> Bool
prop_post_shoot gs = length (projectiles (shoot gs)) == length (projectiles gs) + 1


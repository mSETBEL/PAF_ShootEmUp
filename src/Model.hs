module Model where


-- Types de données pour le jeu
data GameState = GameState { lost :: Bool
                           , player :: Player
                           , projectiles :: [Projectile]
                           , enemies :: [Ennemy]
                           , ennemySpawnTimer :: Int                           
                           , scrollOffset :: Float
                           , bonuses :: [Bonus]
                           , bonusSpawnTimer :: Int
                           , score :: Int
                           }
  deriving (Show)

data Player = Player {
                      persoSpeed :: Float
                    , persoHitbox :: Hitbox
                    , persoHealth :: Int
                    , invincibleTimer :: Int
                    , speedyTimer :: Int
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

data Bonus = Bonus {
                      bonusHitbox :: Hitbox
                    , bonusType :: BonusType
                    , bonusDuration :: Maybe Int
                    , bonusPhase :: Float
                    }
  deriving (Show)

data Direction = LeftDir | RightDir | UpDir | DownDir
  deriving (Show, Eq)
data EnemyType = Red | Green | Blue | Yellow 
  deriving (Show, Eq)
data ProjType = Bullet | Tear
  deriving (Show, Eq)
data BonusType = Health | Speed | Invincibility
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
ennemySpawnSpeed = 1500

bonusSpawnSpeed :: Int
bonusSpawnSpeed = 3000
speedBonusDuration :: Int
speedBonusDuration = 500
invincibilityBonusDuration :: Int
invincibilityBonusDuration = 500

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
initPlayer = Player 2 (Rectangle (-playerWidth / 2) (-100) playerWidth playerHeight) 5 0 0


initProjectile :: Float -> Float -> Direction  -> ProjType -> Projectile
initProjectile x y dir t = Projectile 5 (Disque x y (projectileCOte / 2)) dir t

initGameState :: GameState
initGameState = GameState False initPlayer [] [] 50 0.0 [] 1200 0


-- mouvements du joueur
moveUp :: GameState -> GameState
moveUp gs@(GameState _ player _ _ _ _ _ _ _) =
  case persoHitbox player of
    Rectangle x y w h ->
      let newY = min (screenHeight / 2 - h ) (y + persoSpeed player)
      in gs { player = player { persoHitbox = Rectangle x newY w h } }
    _ -> gs

moveDown :: GameState -> GameState
moveDown gs@(GameState _ player _ _ _ _ _ _ _) =
  case persoHitbox player of
    Rectangle x y w h ->
      let newY = max (-(screenHeight / 2 )) (y - persoSpeed player)
      in gs { player = player { persoHitbox = Rectangle x newY w h } }
    _ -> gs

moveLeft :: GameState -> GameState
moveLeft gs@(GameState _ player _ _ _ _ _ _ _) =
  case persoHitbox player of
    Rectangle x y w h ->
      let newX = max (-(screenWidth / 2)) (x - persoSpeed player)
      in gs { player = player { persoHitbox = Rectangle newX y w h } }
    _ -> gs

moveRight :: GameState -> GameState
moveRight gs@(GameState _ player _ _ _ _ _ _ _) =
  case persoHitbox player of
    Rectangle x y w h ->
      let newX = min (screenWidth / 2 - w) (x + persoSpeed player)
      in gs { player = player { persoHitbox = Rectangle newX y w h } }
    _ -> gs

-- Projectile stuff
--joueur tire un projectile (vers le haut)
shoot :: GameState -> GameState
shoot gs@(GameState _ (Player _ (Rectangle px py pw ph) _ _ _) projs _ _ _ _ _ _) =
  let newProj =  initProjectile (px+pw/2) (py + ph) UpDir Bullet
  in gs { projectiles = newProj : projs }

-- on bouge les projectiles selon leur direction (gauche et droite pas encore utilisés)
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

-- on enlève les projectiles qui sont sortis de l'écran
cullProjectile :: [Projectile] -> [Projectile] 
cullProjectile = filter (\p -> onScreen p) 
  where
    onScreen (Projectile _ (Disque cx cy r) _ _) =
      cx + r >= -screenWidth / 2 && cx - r <= screenWidth / 2 && cy + r >= -screenHeight / 2 && cy - r <= screenHeight / 2

-- fonction de m-a-j des projectiles : on les bouge et on enlève ceux qui sont sortis de l'écran
updateProjectiles :: GameState -> GameState
updateProjectiles gs =
  let updatedProjs = map moveProjectile (projectiles gs)
      culledProjs = cullProjectile updatedProjs
  in gs { projectiles = culledProjs }


-- enemy stuff
-- fonction générale de déplacement d'un ennemi selon son type
moveEnnemy :: Ennemy -> Hitbox -> (Ennemy, Maybe Projectile)
moveEnnemy ennemy ht = case ennemyType ennemy of
  Red    -> (moveRedEnnemy ennemy, Nothing)
  Yellow -> (moveYellowEnnemy ennemy, Nothing)
  Blue   -> moveBlueEnnemy ennemy
  Green  -> (moveGreenEnnemy ennemy ht, Nothing)

-- mouvements ennemies rouges : ils se déplacent horizontalement et font un mouvement de haut en bas
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

-- mouvements des ennemies jaunes : ils arrivent par le bas en diagonale et rebondissent sur les bords de l'écran
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

-- mouvements des ennemis bleus : ils se déplacent de gauchent à droite en haut de l'écran et tirent des projectiles vers le bas à intervalles réguliers
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

-- mouvements des ennemis verts : ils suivent le joueur 
moveGreenEnnemy :: Ennemy -> Hitbox -> Ennemy
moveGreenEnnemy e@(Ennemy sp (Disque cx cy r) _ _ lives _) (Rectangle playerCx playerCy playerW playerH) =
  let dx      = playerCx - cx
      dy      = playerCy - cy
      dist    = sqrt (dx*dx + dy*dy)
      normDx  = if dist == 0 then 0 else dx / dist
      normDy  = if dist == 0 then 0 else dy / dist
  in e { ennemyHitbox    = Disque (cx + normDx * sp) (cy + normDy * sp) r
       , ennemyDirection = (normDx, normDy) }

-- fonctions constructrices d'ennemis
spawnYellowEnnemy :: Float -> Ennemy
spawnYellowEnnemy playerX = Ennemy 4
                               (Disque spawnX (-screenHeight / 2 - ennemyCote) (ennemyCote / 2))
                               (dir, 1) Yellow 0 False
  where
    seed   = round playerX :: Int
    spawnX = fromIntegral ((seed * 1103515245 + 12345) `mod` round screenWidth) - screenWidth / 2 --arrive par un endroit selon la position du joueur
    dir    = if spawnX < playerX then -1 else 1 

spawnRedEnnemies :: Float -> [Ennemy]
spawnRedEnnemies playerY = map makeOne [0..3]
  where
    side      = if playerY > 0 then 1 else -1
    r         = ennemyCote / 2
    gap       = ennemyCote + 5
    startX    = if side == 1 then (screenWidth / 2) + r else -(screenWidth / 2) - r - 4 * gap -- les ennemies appareissent à droite ou à gauche de l'écran selon la position du joueur
    makeOne i = Ennemy 2
                  (Disque (startX + fromIntegral i * gap) 50 r)
                  (side, 0) Red 
                  (fromIntegral i * pi / 2) False

spawnBlueEnnemies :: [Ennemy]
spawnBlueEnnemies = 
                  [ Ennemy 2
                    (Disque (-screenWidth / 2) 100 (ennemyCote / 2))
                    (1, 0) Blue tearTimer False , 
                  Ennemy 2
                    (Disque (screenWidth / 2) 100 (ennemyCote / 2))
                    (-1, 0) Blue tearTimer False
                  ]       

spawnGreenEnnemy :: Ennemy
spawnGreenEnnemy = Ennemy 1
                    (Disque (screenWidth / 2) (-100) ennemyCote) 
                    (-1, 0) Green 3 False

-- enlever les ennemies tués et les projectiles qui ont tué un ennemi, gère aussi les perte de vie du joueur et l'invincibilité temporaire après une collision
killEnnemy :: GameState -> GameState
killEnnemy gs@(GameState _ player projs enns est _ _ _ _) =
  let updatedGreenEnns = takeGreenLife enns in
  let updatedEnns = filter (not . isKilled) updatedGreenEnns in
  let updatedProj = filter (not . hasKilled) projs
  in gs { enemies = updatedEnns, projectiles = updatedProj, player = newPlayer, lost = newLost }
  where
    newPlayer = if (any (\enn -> collision (ennemyHitbox enn) (persoHitbox player)) enns || any (\proj -> projType proj == Tear && collision (projHitbox proj) (persoHitbox player)) projs) && invincibleTimer player == 0
                then player { persoHealth =max 0 (persoHealth player - 1), invincibleTimer = 70 }
                else player { invincibleTimer = max 0 (invincibleTimer player - 1) }
    newLost = persoHealth newPlayer <= 0
    takeGreenLife = map (\enn -> if ennemyType enn == Green && ((any (\proj -> projType proj == Bullet && collision (projHitbox proj) (ennemyHitbox enn)) projs) || (collision (ennemyHitbox enn) (persoHitbox player) && invincibleTimer player == 0))
                                  then enn { ennemyPhase = ennemyPhase enn - 1 }
                                  else enn)
    isKilled ennemy = case ennemyType ennemy of
      Green -> ennemyPhase ennemy <= 0
      _ -> any (\proj ->  projType proj == Bullet && collision (projHitbox proj) (ennemyHitbox ennemy)) projs ||( collision (ennemyHitbox ennemy) (persoHitbox player) && invincibleTimer player == 0)
    hasKilled proj = any (\ennemy -> projType proj == Bullet && collision (projHitbox proj) (ennemyHitbox ennemy)) enns || (projType proj == Tear && collision (projHitbox proj) (persoHitbox player) && invincibleTimer player == 0)

-- les ennemies spawn en dehors de l'écran, dès qu'ils entrent dans l'écran, ils n'en sortent plus
updateOnScreen :: Ennemy -> Ennemy
updateOnScreen e@(Ennemy _ (Disque cx cy r) _ _ _ onS) =
  e { onScreen = onS ||  -- quand l'ennemi rentre dans l'écran, il reste dans l'écran
        cx - r >= -(screenWidth / 2)
     && cx + r <= screenWidth / 2
     && cy - r >= -(screenHeight / 2)
     && cy + r <= screenHeight / 2 }

-- fonction de m-a-j des ennemis : on les bouge, on gère les tirs des ennemis bleus, on spawn de nouveaux ennemis selon le timer et on enlève ceux qui sont tués
updateEnnemies :: GameState -> GameState
updateEnnemies gs@(GameState _ player projs enns est _ _ _ _) =
   
  let (movedEnns, newProjs) = unzip (map (\e -> moveEnnemy e (persoHitbox player)) enns)
      extraProjs             = [ p | Just p <- newProjs ]
      moved                  = gs { enemies      = movedEnns
                                  , projectiles  = projs ++ extraProjs }
      killed                 = killEnnemy moved
 
  in case est of
    0 ->let py = case persoHitbox player of
                   Rectangle _ y _ _ -> y
                   _                 -> 0
        in killed { enemies = spawnRedEnnemies py ++ enemies killed, ennemySpawnTimer = ennemySpawnSpeed }
    1200 -> let px = case persoHitbox player of
                   Rectangle x _ _ _ -> x
                   _                 -> 0
        in killed { enemies          = spawnYellowEnnemy px : enemies killed
                  , ennemySpawnTimer = est - 1 }
    900 -> killed { enemies = spawnBlueEnnemies ++ enemies killed, ennemySpawnTimer = est-1 }
    
    600 -> killed { enemies = spawnGreenEnnemy : enemies killed, ennemySpawnTimer = est-1 }

    _ -> killed { ennemySpawnTimer = est-1 }


--collision (réponse à la question de l'examen, pas tous utiles)
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


-- pour les murs, trouver le segment correspondant à la position y du point
findSegment :: Float -> [(Float,Float)] -> Maybe (Float,Float,Float,Float)
findSegment py pts = go pts
  where
    go ((x1,y1):(x2,y2):rest)
      | py >= y1 && py < y2 = Just (x1,y1,x2,y2)
      | otherwise            = go ((x2,y2):rest)
    go _ = Nothing

-- scroll de l'arrière plan
updateScroll :: GameState -> GameState
updateScroll gs = gs { scrollOffset = if scrollOffset gs <= -screenHeight then 0 
                                 else scrollOffset gs - scrollSpeed }


--bonus stuff
spawnBonus :: Float -> BonusType -> Bonus
spawnBonus playerX bType =
  Bonus (Disque spawnX (screenHeight / 2 + 20) 10) bType duration 0
  where
    spawnX   = calculateSpawnX playerX
    duration = case bType of
                 Health        -> Nothing
                 Speed         -> Just 500
                 Invincibility -> Just 500
    calculateSpawnX x =
      if x < 0 then min (-x) (screenWidth / 2 - 50)
               else max (-x) (-(screenWidth / 2) + 50)


moveBonus :: Bonus -> Bonus
moveBonus b@(Bonus (Disque cx cy r) t d phase) =
  b { bonusHitbox = Disque cx (cy - 1) r }

-- on enlève les bonus qui sont sortis de l'écran ou ramassés par le joueur
cullBonus :: GameState -> GameState
cullBonus gs@(GameState _ player _ _ _ _ bonuses _ _) =
  let updatedBonuses = filter (not . isCulled) bonuses
  in gs { bonuses = updatedBonuses }
  where
    isCulled bonus@(Bonus (Disque cx cy r) _ _ _) =
      cy + r < -(screenHeight / 2)|| collision (bonusHitbox bonus) (persoHitbox player)   

--on applique les bonus ramassés par le joueur et on enlève ceux qui ont été appliqués
applyBonuses :: GameState -> GameState
applyBonuses gs@(GameState _ player _ _ _ _ bonuses _ _) =
  let (newPlayer, newBonuses) = foldl applyOne (player, []) bonuses
  in gs { player = newPlayer, bonuses = newBonuses }
  where
    applyOne (pl, acc) bonus@(Bonus _ t d _) =
      if collision (bonusHitbox bonus) (persoHitbox pl)
      then let newPl = case t of
                        Health        -> if persoHealth pl < 5 then pl { persoHealth = persoHealth pl + 1 } else pl
                        Speed         -> pl { speedyTimer = (case d of Just dur -> dur; Nothing -> 0) }
                        Invincibility -> pl { invincibleTimer = (case d of Just dur -> dur; Nothing -> 0) }
           in (newPl, acc)  -- le bonus est appliqué et retiré de la liste
      else (pl, bonus : acc)  -- le bonus n'est pas appliqué et reste dans la liste

updateSpeedTimer:: GameState -> GameState
updateSpeedTimer gs@(GameState _ player _ _ _ _ _ _ _) =
  let newPlayer = if speedyTimer player > 0
                  then player { persoSpeed = 3.5 , speedyTimer = speedyTimer player - 1 }
                  else player { persoSpeed = 2 }
  in gs { player = newPlayer }

updateBonuses :: GameState -> GameState
updateBonuses gs@(GameState _ player _ _ _ _ b bst _) =
  let movedBonuses = map moveBonus b
      applied      = applyBonuses gs { bonuses = movedBonuses }  -- appliquer les bonus avant de les enlever
      culled       = cullBonus applied
      px           = case persoHitbox player of
                       Rectangle x _ _ _ -> x
                       _                 -> 0
  in case bst of
       0    -> culled { bonusSpawnTimer = bonusSpawnSpeed
                       , bonuses = spawnBonus px Health : bonuses culled }
       1000 -> culled { bonusSpawnTimer = bst - 1
                       , bonuses = spawnBonus px Speed : bonuses culled }
       2000 -> culled { bonusSpawnTimer = bst - 1
                       , bonuses = spawnBonus px Invincibility : bonuses culled }
       _    -> culled { bonusSpawnTimer = bst - 1 }

-- preconditions movements
prop_pre_moveLeft :: GameState -> Bool
prop_pre_moveLeft (GameState _ (Player _ (Rectangle x _ w _) _ _ _) _ _ _ _ _ _ _) =
  x > -(screenWidth / 2 )
prop_pre_moveLeft _ = False

prop_pre_moveRight :: GameState -> Bool
prop_pre_moveRight (GameState _ (Player _ (Rectangle x _ w _) _ _ _) _ _ _ _ _ _ _) =
  x < screenWidth / 2 - w
prop_pre_moveRight _ = False

prop_pre_moveUp :: GameState -> Bool
prop_pre_moveUp (GameState _ (Player _ (Rectangle _ y _ h) _ _ _) _ _ _ _ _ _ _) =
  y < screenHeight / 2 - h 
prop_pre_moveUp _ = False

prop_pre_moveDown :: GameState -> Bool
prop_pre_moveDown (GameState _ (Player _ (Rectangle _ y _ h) _ _ _) _ _ _ _ _ _ _) =
  y > -(screenHeight / 2 )
prop_pre_moveDown _ = False


-- preconditions shoot
prop_pre_shoot :: GameState -> Bool
prop_pre_shoot (GameState _ (Player _ (Rectangle _ _ _ _) _ _ _) _ _ _ _ _ _ _) = True
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
prop_inv_player (GameState _ (Player sp (Rectangle px py pw ph) hp inv speedyTimer) _ _ _ _ _ _ _) =
  sp > 0 
  && px >= -(screenWidth / 2) && px <= screenWidth / 2 - pw
  && py >= -(screenHeight / 2) && py <= screenHeight / 2 - ph 
  && hp >= 0
  && inv >= 0
  && speedyTimer >= 0
prop_inv_player _ = False

-- les projectiles doivent être dans les limites de l'écran
prop_inv_projectiles :: GameState -> Bool
prop_inv_projectiles (GameState _ _ projs _ _ _ _ _ _) =
  all valid projs
  where
    valid (Projectile sp (Disque cx cy r) _ _) =
      sp > 0
      && cx + r >= -(screenWidth / 2) && cx - r <= screenWidth / 2
      && cy + r >= -(screenHeight / 2) && cy - r <= screenHeight / 2
    valid _ = False

-- les ennemis doivent être dans les limites de l'écran
prop_inv_enemies :: GameState -> Bool
prop_inv_enemies (GameState _  _ _ enns _ _ _ _ _) =
  all valid enns
  where
    valid (Ennemy sp (Disque cx cy r) _ _ _ True) =
      sp > 0
      && cx + r >= -(screenWidth / 2) && cx - r <= screenWidth / 2
      && cy + r >= -(screenHeight / 2) && cy - r <= screenHeight / 2
    valid (Ennemy sp _ _ _ _ False) = sp > 0 -- les ennemis hors de l'écran n'ont pas de contraintes de position
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
prop_post_moveUp gs@(GameState _ (Player sp (Rectangle x y w h) _ _ _) _ _ _ _ _ _ _) =
  case moveUp gs of
    GameState _ (Player _ (Rectangle _ y2 _ _) _ _ _) _ _ _ _ _ _ _ ->
      if prop_pre_moveUp gs
      then y2 == min (screenHeight / 2 - h) (y + sp)  -- clamped
      else y2 == y
    _ -> False
prop_post_moveUp _ = False

prop_post_moveDown :: GameState -> Bool
prop_post_moveDown gs@(GameState _ (Player sp (Rectangle x y w h) _ _ _) _ _ _ _ _ _ _) =
  case moveDown gs of
    GameState _ (Player _ (Rectangle _ y2 _ _) _ _ _) _ _ _ _ _ _ _ ->
      if prop_pre_moveDown gs
      then y2 == max (-(screenHeight / 2 )) (y - sp)  -- clamped
      else y2 == y
    _ -> False
prop_post_moveDown _ = False


prop_post_moveLeft :: GameState -> Bool
prop_post_moveLeft gs@(GameState _ (Player sp (Rectangle x y w h) _ _ _) _ _ _ _ _ _ _) =
  case moveLeft gs of
    GameState _ (Player _ (Rectangle x2 _ _ _) _ _ _) _ _ _ _ _ _ _ ->
      if prop_pre_moveLeft gs
      then x2 == max (-(screenWidth / 2 )) (x - sp)
      else x2 == x
    _ -> False
prop_post_moveLeft _ = False

prop_post_moveRight :: GameState -> Bool
prop_post_moveRight gs@(GameState _ (Player sp (Rectangle x y w h) _ _ _) _ _ _ _ _ _ _) =
  case moveRight gs of
    GameState _ (Player _ (Rectangle x2 _ _ _) _ _ _) _ _ _ _ _ _ _ ->
      if prop_pre_moveRight gs
      then x2 == min (screenWidth / 2 - w ) (x + sp)
      else x2 == x
    _ -> False
prop_post_moveRight _ = False


-- postconditions shoot (le nombre de projectiles doit augmenter de 1)
prop_post_shoot :: GameState -> Bool
prop_post_shoot gs = length (projectiles (shoot gs)) == length (projectiles gs) + 1


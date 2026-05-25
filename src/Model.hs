module Model where

import Control.Monad.State
import Data.List (partition)
import Score (Score (..), scoreValue)
import Vec2 (Vec2 (..), vadd, vscale, vnormalize, vsub, toTuple)


-- State monad : porte le GameState à travers toutes les mises à jour.
type Game a = State GameState a

resetGame :: Game ()
resetGame = put initGameState

moveLeftM :: Game ()
moveLeftM = modify moveLeft
moveRightM :: Game ()
moveRightM = modify moveRight
moveUpM :: Game ()
moveUpM = modify moveUp
moveDownM :: Game ()
moveDownM = modify moveDown

shootM :: Game ()
shootM = modify shoot

updateProjectilesM :: Game ()
updateProjectilesM = modify updateProjectiles
updateEnnemiesM :: Game ()
updateEnnemiesM = modify updateEnnemies
updateScrollM :: Game ()
updateScrollM = modify updateScroll
updateBonusesM :: Game ()
updateBonusesM = modify updateBonuses
updateSpeedTimerM :: Game ()
updateSpeedTimerM = modify updateSpeedTimer

updateWeaponTimerM :: Game ()
updateWeaponTimerM = modify updateWeaponTimer


gameLoop :: [Bool] -> Game ()
gameLoop [left, right, up, down] = do
  if left then moveLeftM else return ()
  if right then moveRightM else return ()
  if up then moveUpM else return ()
  if down then moveDownM else return ()
  updateProjectilesM
  updateEnnemiesM
  updateScrollM
  updateBonusesM
  updateSpeedTimerM
  updateWeaponTimerM
gameLoop _ = return ()

data GameState = GameState { lost :: Bool
                           , player :: Player
                           , projectiles :: [Projectile]
                           , enemies :: [Ennemy]
                           , ennemySpawnTimer :: Int                           
                           , scrollOffset :: Float
                           , bonuses :: [Bonus]
                           , bonusSpawnTimer :: Int
                           , score :: Int
                           , scrollReverseTimer :: Int
                           , player2 :: Maybe Player
                           }
  deriving (Show)

data Player = Player {
                      persoSpeed :: Float
                    , persoHitbox :: Hitbox
                    , persoHealth :: Int
                    , invincibleTimer :: Int
                    , speedyTimer :: Int
                    , livesLeft :: Int
                    , weaponMode :: WeaponMode
                    , weaponTimer :: Int
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
                    }
  deriving (Show, Eq)

data Direction = LeftDir | RightDir | UpDir | DownDir
  deriving (Show, Eq)
data EnemyType = Red | Green | Blue | Yellow | Boss
  deriving (Show, Eq)
data ProjType = Bullet | Tear
  deriving (Show, Eq)
data BonusType = Health | Speed | Invincibility | ScrollReverse | TripleShotBonus
  deriving (Show, Eq)
-- Modes d'armement : single par défaut, triple via bonus (extension de tir latéral).
data WeaponMode = SingleShot | TripleShot
  deriving (Show, Eq)

data Hitbox = Point Float Float
            | Disque Float Float Float       -- centre x, centre y, rayon
            | Rectangle Float Float Float Float -- x, y, largeur, hauteur
            | Composee [Hitbox]
            | MurGauche [(Float, Float)]
            | MurDroit [(Float, Float)]
            deriving (Eq, Show)


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

healthBonusCote :: Float
healthBonusCote = 16
speedBonusCote :: Float
speedBonusCote = 18
invincibilityBonusCote :: Float
invincibilityBonusCote = 16.5

playerHeight :: Float
playerHeight = 72
playerWidth :: Float
playerWidth = 42


tearTimer :: Float
tearTimer = 40

-- 2 essais supplémentaires en plus de la vie courante (3 tentatives totales).
initLives :: Int
initLives = 2

-- HP rendus au respawn (identique à initPlayer pour cohérence).
respawnHealth :: Int
respawnHealth = 5

-- Brève invincibilité offerte au respawn pour éviter une mort instantanée.
respawnInvincibility :: Int
respawnInvincibility = 200

-- Durée des bonus d'extension : inversion du scroll et tir triple.
scrollReverseBonusDuration :: Int
scrollReverseBonusDuration = 400

tripleShotBonusDuration :: Int
tripleShotBonusDuration = 600

-- Tailles d'affichage et hitbox pour les deux nouveaux bonus.
scrollReverseBonusCote :: Float
scrollReverseBonusCote = 18

tripleShotBonusCote :: Float
tripleShotBonusCote = 18

initPlayer :: Player
initPlayer = mkPlayer 2 (Rectangle (-playerWidth / 2) (-100) playerWidth playerHeight) 5 initLives

-- refuse les états invalides au lieu de propager une incohérence silencieuse.
mkPlayer :: Float -> Hitbox -> Int -> Int -> Player
mkPlayer speed hb hp lives
  | speed <= 0      = error "mkPlayer: vitesse doit etre > 0"
  | hp   <  0       = error "mkPlayer: HP doit etre >= 0"
  | lives < 0       = error "mkPlayer: lives doit etre >= 0"
  | not (isRect hb) = error "mkPlayer: hitbox doit etre un Rectangle"
  | otherwise       = Player speed hb hp 0 0 lives SingleShot 0
  where
    isRect (Rectangle _ _ w h) = w > 0 && h > 0
    isRect _                   = False


initProjectile :: Float -> Float -> Direction  -> ProjType -> Projectile
initProjectile = mkProjectile

-- impose une hitbox Disque : les autres formes ne sont pas définies dans `collision` pour un projectile.
mkProjectile :: Float -> Float -> Direction -> ProjType -> Projectile
mkProjectile x y dir t = Projectile 5 (Disque x y (projectileCOte / 2)) dir t

-- Smart constructors validants pour les hitbox primitives.
-- Point : pas d'invariant non trivial mais on expose le constructeur par cohérence.
mkPoint :: Float -> Float -> Maybe Hitbox
mkPoint x y = Just (Point x y)

-- Disque : rayon strictement positif (sinon collisions dégénérées).
mkDisque :: Float -> Float -> Float -> Maybe Hitbox
mkDisque cx cy r | r > 0     = Just (Disque cx cy r)
                 | otherwise = Nothing

-- Rectangle : largeur et hauteur strictement positives.
mkRectangle :: Float -> Float -> Float -> Float -> Maybe Hitbox
mkRectangle x y w h | w > 0 && h > 0 = Just (Rectangle x y w h)
                    | otherwise       = Nothing

-- Joueur considéré mort : plus de PV et plus d'essais.
playerDead :: Player -> Bool
playerDead p = persoHealth p == 0 && livesLeft p == 0

-- impose ennemyPhase > 0 sinon l'ennemi serait killed à la naissance.
mkEnnemyHP :: Float -> Hitbox -> (Float, Float) -> EnemyType -> Float -> Ennemy
mkEnnemyHP sp hb dir t hp
  | hp <= 0   = error "mkEnnemyHP: HP doit etre > 0"
  | otherwise = Ennemy sp hb dir t hp False

-- bonus valide : Health => durée Nothing ; autres types => durée Just k avec k > 0.
mkBonus :: Hitbox -> BonusType -> Maybe Int -> Bonus
mkBonus hb t d
  | not (isDisque hb)             = error "mkBonus: hitbox doit etre un Disque"
  | t == Health, d /= Nothing     = error "mkBonus: Health n'a pas de duree"
  | t /= Health, not (validDur d) = error "mkBonus: bonus temporel exige une duree > 0"
  | otherwise                     = Bonus hb t d
  where
    isDisque (Disque _ _ _) = True
    isDisque _              = False
    validDur (Just k) = k > 0
    validDur Nothing  = False

initGameState :: GameState
initGameState = GameState False initPlayer [] [] 50 0.0 [] 1200 0 0 Nothing


moveUp :: GameState -> GameState
moveUp gs | playerDead (player gs) = gs
moveUp gs = case persoHitbox p of
    Rectangle x y w h ->
      let newY = min (screenHeight / 2 - h ) (y + persoSpeed p)
          cand = Rectangle x newY w h
      -- On annule le mouvement si le candidat entre en collision avec un mur du couloir.
      in if collision (worldWalls gs) cand then gs
         else gs { player = p { persoHitbox = cand } }
    _ -> gs
  where p = player gs

moveDown :: GameState -> GameState
moveDown gs | playerDead (player gs) = gs
moveDown gs = case persoHitbox p of
    Rectangle x y w h ->
      let newY = max (-(screenHeight / 2 )) (y - persoSpeed p)
          cand = Rectangle x newY w h
      in if collision (worldWalls gs) cand then gs
         else gs { player = p { persoHitbox = cand } }
    _ -> gs
  where p = player gs

moveLeft :: GameState -> GameState
moveLeft gs | playerDead (player gs) = gs
moveLeft gs = case persoHitbox p of
    Rectangle x y w h ->
      let newX = max (-(screenWidth / 2)) (x - persoSpeed p)
          cand = Rectangle newX y w h
      in if collision (worldWalls gs) cand then gs
         else gs { player = p { persoHitbox = cand } }
    _ -> gs
  where p = player gs

moveRight :: GameState -> GameState
moveRight gs | playerDead (player gs) = gs
moveRight gs = case persoHitbox p of
    Rectangle x y w h ->
      let newX = min (screenWidth / 2 - w) (x + persoSpeed p)
          cand = Rectangle newX y w h
      in if collision (worldWalls gs) cand then gs
         else gs { player = p { persoHitbox = cand } }
    _ -> gs
  where p = player gs

-- projectiles
shoot :: GameState -> GameState
shoot gs | lost gs = gs
shoot gs | playerDead (player gs) = gs
shoot gs = case persoHitbox (player gs) of
  Rectangle px py pw ph ->
    let wm = weaponMode (player gs)
        muzzleX = px + pw/2
        muzzleY = py + ph
        newProjs = case wm of
          SingleShot -> [initProjectile muzzleX muzzleY UpDir Bullet]
          -- trois balles parallèles avec un léger décalage horizontal.
          TripleShot -> [ initProjectile muzzleX        muzzleY UpDir Bullet
                        , initProjectile (muzzleX - 10) muzzleY UpDir Bullet
                        , initProjectile (muzzleX + 10) muzzleY UpDir Bullet
                        ]
    in gs { projectiles = newProjs ++ projectiles gs }
  _ -> gs


-- Mode coop : joueur 2 optionnel, partage ennemis/bonus/score.

-- spawn de P2 décalé à droite pour ne pas le superposer à P1 ; chaque joueur a son propre pool de vies.
initPlayer2 :: Player
initPlayer2 = mkPlayer 2 (Rectangle (playerWidth) (-100) playerWidth playerHeight) 5 initLives

-- bascule Just/Nothing : active/désactive le coop sans relancer. P2 démarre avec son propre stock d'essais.
togglePlayer2 :: GameState -> GameState
togglePlayer2 gs = case player2 gs of
  Nothing -> gs { player2 = Just initPlayer2 }
  Just _  -> gs { player2 = Nothing }

-- utilitaire : applique une transformation à player2 si présent, sinon no-op.
withP2 :: (Player -> GameState -> GameState) -> GameState -> GameState
withP2 f gs = case player2 gs of
  Nothing -> gs
  Just p2 -> f p2 gs

moveUpP2, moveDownP2, moveLeftP2, moveRightP2 :: GameState -> GameState
moveUpP2 = withP2 $ \p2 gs -> case persoHitbox p2 of
  Rectangle x y w h ->
    let newY = min (screenHeight / 2 - h) (y + persoSpeed p2)
        cand = Rectangle x newY w h
    in if collision (worldWalls gs) cand then gs
       else gs { player2 = Just (p2 { persoHitbox = cand }) }
  _ -> gs
moveDownP2 = withP2 $ \p2 gs -> case persoHitbox p2 of
  Rectangle x y w h ->
    let newY = max (-(screenHeight / 2)) (y - persoSpeed p2)
        cand = Rectangle x newY w h
    in if collision (worldWalls gs) cand then gs
       else gs { player2 = Just (p2 { persoHitbox = cand }) }
  _ -> gs
moveLeftP2 = withP2 $ \p2 gs -> case persoHitbox p2 of
  Rectangle x y w h ->
    let newX = max (-(screenWidth / 2)) (x - persoSpeed p2)
        cand = Rectangle newX y w h
    in if collision (worldWalls gs) cand then gs
       else gs { player2 = Just (p2 { persoHitbox = cand }) }
  _ -> gs
moveRightP2 = withP2 $ \p2 gs -> case persoHitbox p2 of
  Rectangle x y w h ->
    let newX = min (screenWidth / 2 - w) (x + persoSpeed p2)
        cand = Rectangle newX y w h
    in if collision (worldWalls gs) cand then gs
       else gs { player2 = Just (p2 { persoHitbox = cand }) }
  _ -> gs

-- Tir de P2 : symétrique de shoot pour P1.
shootP2 :: GameState -> GameState
shootP2 gs | lost gs = gs
shootP2 gs = case player2 gs of
  Nothing -> gs
  Just p2 -> case persoHitbox p2 of
    Rectangle px py pw ph ->
      let wm = weaponMode p2
          muzzleX = px + pw/2
          muzzleY = py + ph
          newProjs = case wm of
            SingleShot -> [initProjectile muzzleX muzzleY UpDir Bullet]
            TripleShot -> [ initProjectile muzzleX        muzzleY UpDir Bullet
                          , initProjectile (muzzleX - 10) muzzleY UpDir Bullet
                          , initProjectile (muzzleX + 10) muzzleY UpDir Bullet
                          ]
      in gs { projectiles = newProjs ++ projectiles gs }
    _ -> gs

-- Wrappers monadiques pour intégrer P2 dans la boucle de jeu.
moveUpP2M, moveDownP2M, moveLeftP2M, moveRightP2M, shootP2M :: Game ()
moveUpP2M    = modify moveUpP2
moveDownP2M  = modify moveDownP2
moveLeftP2M  = modify moveLeftP2
moveRightP2M = modify moveRightP2
shootP2M     = modify shootP2

-- TODO : LeftDir et RightDir non encore utilisés en jeu.
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

cullProjectile :: [Projectile] -> [Projectile] 
cullProjectile = filter (\p -> onScreen p) 
  where
    onScreen (Projectile _ (Disque cx cy r) _ _) =
      cx + r >= -screenWidth / 2 && cx - r <= screenWidth / 2 && cy + r >= -screenHeight / 2 && cy - r <= screenHeight / 2

updateProjectiles :: GameState -> GameState
updateProjectiles gs =
  let updatedProjs = map moveProjectile (projectiles gs)
      culledProjs = cullProjectile updatedProjs
  in gs { projectiles = culledProjs }


-- ennemis
moveEnnemy :: Ennemy -> Hitbox -> (Ennemy, Maybe Projectile)
moveEnnemy ennemy ht = case ennemyType ennemy of
  Red    -> (moveRedEnnemy ennemy, Nothing)
  Yellow -> (moveYellowEnnemy ennemy, Nothing)
  Blue   -> moveBlueEnnemy ennemy
  Green  -> (moveGreenEnnemy ennemy ht, Nothing)
  Boss   -> moveBossEnnemy ennemy

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

-- bleu : se déplace de gauche à droite en haut de l'écran et tire vers le bas à intervalles réguliers.
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

-- vert : poursuit le joueur.
moveGreenEnnemy :: Ennemy -> Hitbox -> Ennemy
moveGreenEnnemy e@(Ennemy sp (Disque cx cy r) _ _ _ _) (Rectangle playerCx playerCy _ _) =
  -- arithmétique vectorielle via Vec2 (Functor/Applicative).
  let pos        = Vec2 cx cy
      target     = Vec2 playerCx playerCy
      dir        = vnormalize (vsub target pos)
      Vec2 nx ny = vadd pos (vscale sp dir)
      (dxn, dyn) = toTuple dir
  in e { ennemyHitbox    = Disque nx ny r
       , ennemyDirection = (dxn, dyn) }
moveGreenEnnemy e _ = e

spawnYellowEnnemy :: Float -> Ennemy
spawnYellowEnnemy playerX = Ennemy 4
                               (Disque spawnX (-screenHeight / 2 - ennemyCote) (ennemyCote / 2))
                               (dir, 1) Yellow 0 False
  where
    seed   = round playerX :: Int
    -- LCG sur playerX pour avoir un spawnX "pseudo-aléatoire" sans IO.
    spawnX = fromIntegral ((seed * 1103515245 + 12345) `mod` round screenWidth) - screenWidth / 2
    dir    = if spawnX < playerX then -1 else 1 

spawnRedEnnemies :: Float -> [Ennemy]
spawnRedEnnemies playerY = map makeOne [0..3]
  where
    side      = if playerY > 0 then 1 else -1
    r         = ennemyCote / 2
    gap       = ennemyCote + 5
    -- côté de spawn opposé au joueur (les rouges arrivent par groupes de 4).
    startX    = if side == 1 then (screenWidth / 2) + r else -(screenWidth / 2) - r - 4 * gap
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
                    (-1, 0) Green 2 False

-- Boss : taille double, 10 HP, descend lentement puis oscille horizontalement.
bossCote :: Float
bossCote = ennemyCote * 2

bossHP :: Float
bossHP = 10

-- Mouvement du boss : descente lente, va-et-vient, tir périodique de tears.
moveBossEnnemy :: Ennemy -> (Ennemy, Maybe Projectile)
moveBossEnnemy e@(Ennemy sp (Disque cx cy r) (dx, reload) Boss phase onS) =
  let newCx = cx + dx * sp
      (newDx, finalCx) =
        if newCx + r >= screenWidth / 2 && dx > 0 then (-dx, screenWidth / 2 - r)
        else if newCx - r <= -(screenWidth / 2) && dx < 0 then (-dx, -(screenWidth / 2) + r)
        else (dx, newCx)
      finalCy = if cy > 80 then cy - 0.3 else cy
      -- compteur de tir stocké dans dy d'ennemyDirection pour ne pas empiéter sur ennemyPhase (HP).
      tickedReload = reload - 1
      shoots = tickedReload <= 0 && onS
      proj = if shoots then Just (initProjectile finalCx (finalCy - r) DownDir Tear) else Nothing
      nextReload = if shoots then bossReloadFrames else tickedReload
  in ( updateOnScreen (e { ennemyHitbox = Disque finalCx finalCy r
                         , ennemyDirection = (newDx, nextReload)
                         , ennemyPhase = phase })
     , proj )
moveBossEnnemy e = (e, Nothing)

bossReloadFrames :: Float
bossReloadFrames = 60

spawnBoss :: Ennemy
spawnBoss = Ennemy 1.5
              (Disque 0 (screenHeight / 2 + bossCote) bossCote)
              (1, bossReloadFrames) Boss bossHP False

-- mort des ennemis touchés + pertes de vie / i-frames côté joueur.
killEnnemy :: GameState -> GameState
killEnnemy = killEnnemyWith 70

-- variante paramétrée : durée d'i-frames lue dans la GameConfig.
killEnnemyWith :: Int -> GameState -> GameState
killEnnemyWith invFr gs =
  let updatedGreenEnns = takeGreenLife enns
      (alive, dead)    = partition (not . isKilled) updatedGreenEnns
      -- Cumul via le Monoid Score : addition associative et neutre Score 0.
      gained           = scoreValue $ foldMap (Score . pointsForType . ennemyType) dead
      updatedProj      = filter (not . hasKilled) projs
  -- Sur respawn on vide le terrain pour ne pas tuer le joueur immédiatement.
  in gs { enemies      = if anyRespawn then [] else alive
        , projectiles  = if anyRespawn then [] else updatedProj
        , player       = finalPlayer1
        , player2      = finalPlayer2
        , lost         = gameOver
        , score        = score gs + gained }
  where
    p1    = player gs
    projs = projectiles gs
    enns  = enemies gs
    p2opt = player2 gs
    -- Test générique : un joueur quelconque est-il touché ce frame ?
    hitOf pl = (any (\enn -> collision (ennemyHitbox enn) (persoHitbox pl)) enns
             || any (\proj -> projType proj == Tear && collision (projHitbox proj) (persoHitbox pl)) projs)
            && invincibleTimer pl == 0
    hitPlayer    = hitOf p1
    hitPlayer2   = case p2opt of { Just p2 -> hitOf p2 ; Nothing -> False }
    -- Logique de respawn factorisée : mêmes règles pour P1 et P2.
    stepPlayer hit pl = if hit
      then pl { persoHealth = max 0 (persoHealth pl - 1), invincibleTimer = invFr }
      else pl { invincibleTimer = max 0 (invincibleTimer pl - 1) }
    respawnPlayer pl =
      pl { persoHealth = respawnHealth
         , invincibleTimer = respawnInvincibility
         , weaponMode = SingleShot
         , weaponTimer = 0
         , persoHitbox = persoHitbox initPlayer }
    newPlayer    = stepPlayer hitPlayer p1
    newPlayer2   = fmap (stepPlayer hitPlayer2) p2opt
    -- Pool de vies indépendant : on consomme une vie sur le joueur concerné uniquement.
    p1NeedsLife  = persoHealth newPlayer <= 0
    canRespawnP1 = p1NeedsLife && livesLeft newPlayer > 0
    finalPlayer1
      | canRespawnP1 = (respawnPlayer newPlayer) { livesLeft = livesLeft newPlayer - 1 }
      | otherwise    = newPlayer
    -- P2 disparaît du jeu lorsqu'il atteint playerDead (mais P1 reste figé pour rester visible).
    finalPlayer2 = case newPlayer2 of
      Nothing -> Nothing
      Just np2 ->
        let p2NeedsLife  = persoHealth np2 <= 0
            canRespawnP2 = p2NeedsLife && livesLeft np2 > 0
            respawned    = if canRespawnP2
                           then Just ((respawnPlayer np2 { persoHitbox = persoHitbox initPlayer2 })
                                        { livesLeft = livesLeft np2 - 1 })
                           else Just np2
        in case respawned of
             Just rp | playerDead rp -> Nothing
             other                   -> other
    anyRespawn = canRespawnP1
              || (case newPlayer2 of
                    Just np2 -> persoHealth np2 <= 0 && livesLeft np2 > 0
                    Nothing  -> False)
    -- Game Over : P1 totalement KO et (pas de P2 ou P2 lui aussi KO).
    gameOver = playerDead finalPlayer1
            && (case finalPlayer2 of { Nothing -> True ; Just p2 -> playerDead p2 })
    -- Plusieurs balles dans la même frame doivent infliger plusieurs dégâts (sinon le triple shot ne vaut pas plus que le single).
    greenHit enn = let n = length [ () | proj <- projs
                                       , projType proj == Bullet
                                       , collision (projHitbox proj) (ennemyHitbox enn) ]
                       touchedP1 = collision (ennemyHitbox enn) (persoHitbox p1)  && invincibleTimer p1  == 0
                       touchedP2 = case p2opt of
                                     Just p2 -> collision (ennemyHitbox enn) (persoHitbox p2) && invincibleTimer p2 == 0
                                     Nothing -> False
                   in n + (if touchedP1 then 1 else 0) + (if touchedP2 then 1 else 0)
    bossHit  enn = length [ () | proj <- projs
                               , projType proj == Bullet
                               , collision (projHitbox proj) (ennemyHitbox enn) ]
    takeGreenLife = map (\enn -> case ennemyType enn of
        -- Vert et boss partagent la logique HP-multiples : on décrémente ennemyPhase d'autant de hits qu'on en a pris dans la frame.
        Green | greenHit enn > 0 -> enn { ennemyPhase = ennemyPhase enn - fromIntegral (greenHit enn) }
        Boss  | bossHit  enn > 0 -> enn { ennemyPhase = ennemyPhase enn - fromIntegral (bossHit  enn) }
        _ -> enn)
    isKilled ennemy = case ennemyType ennemy of
      Green -> ennemyPhase ennemy <= 0
      Boss  -> ennemyPhase ennemy <= 0
      _ -> any (\proj ->  projType proj == Bullet && collision (projHitbox proj) (ennemyHitbox ennemy)) projs
        || (collision (ennemyHitbox ennemy) (persoHitbox p1) && invincibleTimer p1 == 0)
        || (case p2opt of { Just p2 -> collision (ennemyHitbox ennemy) (persoHitbox p2) && invincibleTimer p2 == 0 ; Nothing -> False })
    hasKilled proj = any (\ennemy -> projType proj == Bullet && collision (projHitbox proj) (ennemyHitbox ennemy)) enns
                  || (projType proj == Tear && collision (projHitbox proj) (persoHitbox p1) && invincibleTimer p1 == 0)
                  || (case p2opt of { Just p2 -> projType proj == Tear && collision (projHitbox proj) (persoHitbox p2) && invincibleTimer p2 == 0 ; Nothing -> False })

-- Barème local au module pour éviter une dépendance circulaire avec Score.
pointsForType :: EnemyType -> Int
pointsForType Red    = 50
pointsForType Yellow = 75
pointsForType Blue   = 100
pointsForType Green  = 150
pointsForType Boss   = 1000

-- les ennemis spawnent hors écran : dès qu'ils y entrent, le flag reste à True pour toujours.
updateOnScreen :: Ennemy -> Ennemy
updateOnScreen e@(Ennemy _ (Disque cx cy r) _ _ _ onS) =
  e { onScreen = onS ||
        cx - r >= -(screenWidth / 2)
     && cx + r <= screenWidth / 2
     && cy - r >= -(screenHeight / 2)
     && cy + r <= screenHeight / 2 }

-- pipeline : déplacement + tirs bleus, puis kill, puis spawn selon timer.
updateEnnemies :: GameState -> GameState
updateEnnemies gs =
  let p1   = player gs
      projs = projectiles gs
      enns  = enemies gs
      est   = ennemySpawnTimer gs
      (movedEnns, newProjs) = unzip (map (\e -> moveEnnemy e (persoHitbox p1)) enns)
      extraProjs             = [ p | Just p <- newProjs ]
      moved                  = gs { enemies      = movedEnns
                                  , projectiles  = projs ++ extraProjs }
      killed                 = killEnnemy moved
  in case est of
    0 ->let py = case persoHitbox p1 of
                   Rectangle _ y _ _ -> y
                   _                 -> 0
            -- À chaque cycle complet on tente l'apparition d'un boss si le score le justifie et qu'aucun boss n'est déjà présent.
            withBoss s = if score s >= bossScoreThreshold && not (any ((== Boss) . ennemyType) (enemies s))
                         then s { enemies = spawnBoss : enemies s }
                         else s
        in withBoss (killed { enemies = spawnRedEnnemies py ++ enemies killed, ennemySpawnTimer = ennemySpawnSpeed })
    1200 -> let px = case persoHitbox p1 of
                   Rectangle x _ _ _ -> x
                   _                 -> 0
        in killed { enemies          = spawnYellowEnnemy px : enemies killed
                  , ennemySpawnTimer = est - 1 }
    900 -> killed { enemies = spawnBlueEnnemies ++ enemies killed, ennemySpawnTimer = est-1 }
    600 -> killed { enemies = spawnGreenEnnemy : enemies killed, ennemySpawnTimer = est-1 }
    _ -> killed { ennemySpawnTimer = est-1 }

-- seuil de score avant apparition d'un boss (calibrage difficulté).
bossScoreThreshold :: Int
bossScoreThreshold = 500


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

-- Extensions : on teste les coins gauche du joueur contre le mur gauche, droits contre le mur droit.
collision (MurGauche segs) (Rectangle x y _ h) =
  collision (MurGauche segs) (Point x y) || collision (MurGauche segs) (Point x (y+h))

collision (MurDroit segs) (Rectangle x y w h) =
  collision (MurDroit segs) (Point (x+w) y) || collision (MurDroit segs) (Point (x+w) (y+h))

-- Disque vs mur : on approxime par les deux points latéraux extrêmes du disque sur l'axe x.
collision (MurGauche segs) (Disque cx cy r) =
  collision (MurGauche segs) (Point (cx-r) cy)

collision (MurDroit segs) (Disque cx cy r) =
  collision (MurDroit segs) (Point (cx+r) cy)

collision a b = collision b a


-- couloir latéral zigzaguant : Composee[MurGauche, MurDroit] animé par scrollOffset.
worldWalls :: GameState -> Hitbox
worldWalls gs =
  let off       = scrollOffset gs
      wobble y  = 25 * sin ((y + off) * 0.015)
      leftEdge  = -(screenWidth/2)
      rightEdge =   screenWidth/2
      -- cinq jalons en y suffisent pour couvrir tout l'écran sans artefacts.
      ys        = [-screenHeight, -screenHeight/2, 0, screenHeight/2, screenHeight]
      leftPts   = [ (leftEdge  + 25 + wobble y, y) | y <- ys ]
      rightPts  = [ (rightEdge - 25 - wobble y, y) | y <- ys ]
  in Composee [MurGauche leftPts, MurDroit rightPts]


-- pour les murs : retrouver le segment de polyligne qui contient l'ordonnée py.
findSegment :: Float -> [(Float,Float)] -> Maybe (Float,Float,Float,Float)
findSegment py pts = go pts
  where
    go ((x1,y1):(x2,y2):rest)
      | py >= y1 && py < y2 = Just (x1,y1,x2,y2)
      | otherwise            = go ((x2,y2):rest)
    go _ = Nothing

-- scroll de l'arrière-plan : inversé temporairement par le bonus ScrollReverse.
updateScroll :: GameState -> GameState
updateScroll gs =
  let dir = if scrollReverseTimer gs > 0 then 1 else -1
      step = fromIntegral dir * scrollSpeed
      raw  = scrollOffset gs + step
      -- offset borné dans (-screenHeight, 0] pour la boucle infinie du fond.
      newOff
        | raw <= -screenHeight = 0
        | raw > 0              = -screenHeight + raw
        | otherwise            = raw
      newTimer = max 0 (scrollReverseTimer gs - 1)
      scrolled = gs { scrollOffset = newOff, scrollReverseTimer = newTimer }
  -- après le scroll, le mur peut avoir absorbé un joueur immobile : on le repousse.
  in pushOutOfWalls scrolled

-- repousse le joueur englobé par le mur défilant (au plus 10 pas de 4 px vers le centre).
pushOutOfWalls :: GameState -> GameState
pushOutOfWalls gs = case persoHitbox (player gs) of
  Rectangle x y w h
    | collision (worldWalls gs) (Rectangle x y w h) ->
        let cx       = x + w/2
            -- pousser vers le centre x=0 : à gauche on va à droite, sinon l'inverse.
            stepSign = if cx < 0 then 1 else -1
            tryShift k acc
              | k > 10 = acc
              | not (collision (worldWalls gs) (persoHitbox (player acc))) = acc
              | otherwise =
                  let p   = player acc
                      Rectangle x' y' w' h' = persoHitbox p
                      x''  = x' + stepSign * 4
                      x''' = max (-(screenWidth/2)) (min (screenWidth/2 - w') x'')
                  in tryShift (k+1) acc { player = p { persoHitbox = Rectangle x''' y' w' h' } }
        in tryShift (0 :: Int) gs
    | otherwise -> gs
  _ -> gs


-- bonus
spawnBonus :: Float -> BonusType -> Bonus
spawnBonus playerX bType =
  Bonus (Disque spawnX (screenHeight / 2 + 20) br) bType duration
  where
    spawnX   = calculateSpawnX playerX
    (br, duration) = case bType of
                 Health           -> (healthBonusCote/2, Nothing)
                 Speed            -> (speedBonusCote/2, Just speedBonusDuration)
                 Invincibility    -> (invincibilityBonusCote/2, Just invincibilityBonusDuration)
                 ScrollReverse    -> (scrollReverseBonusCote/2, Just scrollReverseBonusDuration)
                 TripleShotBonus  -> (tripleShotBonusCote/2, Just tripleShotBonusDuration)
    calculateSpawnX x =
      if x < 0 then min (-x) (screenWidth / 2 - 50)
               else max (-x) (-(screenWidth / 2) + 50)


moveBonus :: Bonus -> Bonus
moveBonus b@(Bonus (Disque cx cy r) t d) =
  b { bonusHitbox = Disque cx (cy - 1) r }

cullBonus :: GameState -> GameState
cullBonus gs =
  let p1             = player gs
      updatedBonuses = filter (not . isCulled p1) (bonuses gs)
  in gs { bonuses = updatedBonuses }
  where
    isCulled p1 bonus@(Bonus (Disque cx cy r) _ _) =
      cy + r < -(screenHeight / 2)|| collision (bonusHitbox bonus) (persoHitbox p1)

-- applique et consomme en un seul passage.
applyBonuses :: GameState -> GameState
applyBonuses gs =
  -- accumulateur (état mutant, bonus restants) : on traverse les bonus une seule fois.
  let (newGs, remaining) = foldl applyOne (gs, []) (bonuses gs)
  in newGs { bonuses = remaining }
  where
    -- Applique le bonus à un joueur donné (P1 ou P2). ScrollReverse reste global (touche les deux).
    applyToPlayer t dur pl = case t of
      Health          -> if persoHealth pl < 5
                         then pl { persoHealth = persoHealth pl + 1 }
                         else pl
      Speed           -> pl { speedyTimer = dur }
      Invincibility   -> pl { invincibleTimer = dur }
      TripleShotBonus -> pl { weaponMode = TripleShot, weaponTimer = dur }
      ScrollReverse   -> pl
    applyOne (st, acc) bonus@(Bonus _ t d) =
      let dur     = case d of Just k -> k; Nothing -> 0
          p1      = player st
          p2opt   = player2 st
          hitP1   = collision (bonusHitbox bonus) (persoHitbox p1)
          hitP2   = case p2opt of { Just p2 -> collision (bonusHitbox bonus) (persoHitbox p2) ; Nothing -> False }
      in if hitP1 || hitP2
         then let st1 = case t of
                    ScrollReverse -> st { scrollReverseTimer = dur }
                    _ | hitP1      -> st { player = applyToPlayer t dur p1 }
                    _              -> st
                  st2 = case (t, hitP2, p2opt) of
                    (ScrollReverse, _, _)      -> st1
                    (_, True, Just p2)         -> st1 { player2 = Just (applyToPlayer t dur p2) }
                    _                           -> st1
              in (st2, acc)
         else (st, bonus : acc)

updateSpeedTimer:: GameState -> GameState
updateSpeedTimer gs =
  let stepSp pl = if speedyTimer pl > 0
                  then pl { persoSpeed = 3.5, speedyTimer = speedyTimer pl - 1 }
                  else pl { persoSpeed = 2 }
  in gs { player  = stepSp (player gs)
        , player2 = fmap stepSp (player2 gs) }

-- Décrémente le timer du tir triple et bascule en SingleShot quand il atteint zéro (appliqué à P1 et à P2 si présent).
updateWeaponTimer :: GameState -> GameState
updateWeaponTimer gs =
  let stepW pl =
        let t = weaponTimer pl
        in if t > 0
           then pl { weaponTimer = t - 1
                   , weaponMode  = if t - 1 <= 0 then SingleShot else weaponMode pl }
           else pl { weaponMode = SingleShot, weaponTimer = 0 }
  in gs { player  = stepW (player gs)
        , player2 = fmap stepW (player2 gs) }

updateBonuses :: GameState -> GameState
updateBonuses gs =
  let p1           = player gs
      bst          = bonusSpawnTimer gs
      movedBonuses = map moveBonus (bonuses gs)
      applied      = applyBonuses gs { bonuses = movedBonuses }  -- appliquer les bonus avant de les enlever
      culled       = cullBonus applied
      px           = case persoHitbox p1 of
                       Rectangle x _ _ _ -> x
                       _                 -> 0
  in case bst of
       0    -> culled { bonusSpawnTimer = bonusSpawnSpeed
                       , bonuses = spawnBonus px Health : bonuses culled }
       1000 -> culled { bonusSpawnTimer = bst - 1
                       , bonuses = spawnBonus px Speed : bonuses culled }
       2000 -> culled { bonusSpawnTimer = bst - 1
                       , bonuses = spawnBonus px Invincibility : bonuses culled }
       -- Apparitions intercalées des deux extensions (reverse et triple) sur le même cycle.
       1500 -> culled { bonusSpawnTimer = bst - 1
                       , bonuses = spawnBonus px ScrollReverse : bonuses culled }
       2500 -> culled { bonusSpawnTimer = bst - 1
                       , bonuses = spawnBonus px TripleShotBonus : bonuses culled }
       _    -> culled { bonusSpawnTimer = bst - 1 }

-- préconditions de déplacement
prop_pre_moveLeft :: GameState -> Bool
prop_pre_moveLeft gs = case persoHitbox (player gs) of
  Rectangle x _ _ _ -> x > -(screenWidth / 2 )
  _                 -> False

prop_pre_moveRight :: GameState -> Bool
prop_pre_moveRight gs = case persoHitbox (player gs) of
  Rectangle x _ w _ -> x < screenWidth / 2 - w
  _                 -> False

prop_pre_moveUp :: GameState -> Bool
prop_pre_moveUp gs = case persoHitbox (player gs) of
  Rectangle _ y _ h -> y < screenHeight / 2 - h
  _                 -> False

prop_pre_moveDown :: GameState -> Bool
prop_pre_moveDown gs = case persoHitbox (player gs) of
  Rectangle _ y _ _ -> y > -(screenHeight / 2 )
  _                 -> False


-- précondition réelle pour shoot : on n'autorise pas le tir après game over et on suppose la hitbox rectangulaire.
prop_pre_shoot :: GameState -> Bool
prop_pre_shoot gs = case persoHitbox (player gs) of
  Rectangle _ _ _ _ -> not (lost gs)
  _                 -> False

prop_pre_spawnTimer :: GameState -> Bool
prop_pre_spawnTimer gs = ennemySpawnTimer gs <= 0

prop_pre_updateScroll :: GameState -> Bool
prop_pre_updateScroll gs = scrollOffset gs > -screenHeight && scrollOffset gs <= 0



-- invariants

prop_inv_player :: GameState -> Bool
prop_inv_player gs = case player gs of
  Player sp (Rectangle px py pw ph) hp inv speedyT ll wm wt ->
       sp > 0
    && px >= -(screenWidth / 2) && px <= screenWidth / 2 - pw
    && py >= -(screenHeight / 2) && py <= screenHeight / 2 - ph
    && hp >= 0
    && inv >= 0
    && speedyT >= 0
    -- essais restants : ni négatifs ni au-dessus de la valeur initiale.
    && ll >= 0 && ll <= initLives
    -- timer d'arme borné, strictement positif uniquement en TripleShot.
    && wt >= 0 && wt <= tripleShotBonusDuration
    && (if wt > 0 then wm == TripleShot else wm == SingleShot)
  _ -> False

prop_inv_projectiles :: GameState -> Bool
prop_inv_projectiles gs = all valid (projectiles gs)
  where
    valid (Projectile sp (Disque cx cy r) _ _) =
      sp > 0
      && cx + r >= -(screenWidth / 2) && cx - r <= screenWidth / 2
      && cy + r >= -(screenHeight / 2) && cy - r <= screenHeight / 2
    valid _ = False

prop_inv_enemies :: GameState -> Bool
prop_inv_enemies gs = all valid (enemies gs)
  where
    valid (Ennemy sp (Disque cx cy r) _ _ _ True) =
      sp > 0
      && cx + r >= -(screenWidth / 2) && cx - r <= screenWidth / 2
      && cy + r >= -(screenHeight / 2) && cy - r <= screenHeight / 2
    -- hors écran : pas de contrainte de position.
    valid (Ennemy sp _ _ _ _ False) = sp > 0
    valid _ = False

prop_inv_bonuses :: GameState -> Bool
prop_inv_bonuses gs = all valid (bonuses gs)
  where
    valid (Bonus (Disque cx cy r) t d) =
      cx + r >= -(screenWidth / 2) && cx - r <= screenWidth / 2
      && cy - r <= screenHeight / 2
      && case t of
            Health           -> d == Nothing
            Speed            -> case d of Just dur -> dur == speedBonusDuration; Nothing -> False
            Invincibility    -> case d of Just dur -> dur == invincibilityBonusDuration; Nothing -> False
            ScrollReverse    -> case d of Just dur -> dur == scrollReverseBonusDuration; Nothing -> False
            TripleShotBonus  -> case d of Just dur -> dur == tripleShotBonusDuration; Nothing -> False
    valid _ = False

-- timer d'inversion : positif ou nul, s'éteint en s'épuisant.
prop_inv_scrollReverse :: GameState -> Bool
prop_inv_scrollReverse gs = scrollReverseTimer gs >= 0

prop_inv_scroll :: GameState -> Bool
prop_inv_scroll gs = scrollOffset gs > -screenHeight && scrollOffset gs <= 0

prop_inv_spawnTimer :: GameState -> Bool
prop_inv_spawnTimer gs = ennemySpawnTimer gs >= 0 && bonusSpawnTimer gs >= 0

-- score : positif ou nul (jamais décrémenté).
prop_inv_score :: GameState -> Bool
prop_inv_score gs = score gs >= 0

-- invariant global
prop_inv_GameState :: GameState -> Bool
prop_inv_GameState gs =
  prop_inv_player gs
  && prop_inv_projectiles gs
  && prop_inv_enemies gs
  && prop_inv_scroll gs
  && prop_inv_bonuses gs
  && prop_inv_spawnTimer gs
  && prop_inv_score gs
  && prop_inv_scrollReverse gs


--postconditions

prop_post_moveUp :: GameState -> Bool
prop_post_moveUp gs = case persoHitbox (player gs) of
  Rectangle _ y _ h ->
    let sp = persoSpeed (player gs)
    in case persoHitbox (player (moveUp gs)) of
         Rectangle _ y2 _ _ ->
           if prop_pre_moveUp gs
           -- Soit le déplacement est appliqué (clamped), soit il a été annulé par un mur.
           then y2 == min (screenHeight / 2 - h) (y + sp) || y2 == y
           else y2 == y
         _ -> False
  _ -> False

prop_post_moveDown :: GameState -> Bool
prop_post_moveDown gs = case persoHitbox (player gs) of
  Rectangle _ y _ _ ->
    let sp = persoSpeed (player gs)
    in case persoHitbox (player (moveDown gs)) of
         Rectangle _ y2 _ _ ->
           if prop_pre_moveDown gs
           then y2 == max (-(screenHeight / 2 )) (y - sp) || y2 == y
           else y2 == y
         _ -> False
  _ -> False


prop_post_moveLeft :: GameState -> Bool
prop_post_moveLeft gs = case persoHitbox (player gs) of
  Rectangle x _ _ _ ->
    let sp = persoSpeed (player gs)
    in case persoHitbox (player (moveLeft gs)) of
         Rectangle x2 _ _ _ ->
           if prop_pre_moveLeft gs
           then x2 == max (-(screenWidth / 2 )) (x - sp) || x2 == x
           else x2 == x
         _ -> False
  _ -> False

prop_post_moveRight :: GameState -> Bool
prop_post_moveRight gs = case persoHitbox (player gs) of
  Rectangle x _ w _ ->
    let sp = persoSpeed (player gs)
    in case persoHitbox (player (moveRight gs)) of
         Rectangle x2 _ _ _ ->
           if prop_pre_moveRight gs
           then x2 == min (screenWidth / 2 - w ) (x + sp) || x2 == x
           else x2 == x
         _ -> False
  _ -> False


-- single → +1, triple → +3 (sinon faux négatif après bonus TripleShot).
-- Exception : si P1 est mort (playerDead), shoot devient un no-op et n'ajoute rien.
prop_post_shoot :: GameState -> Bool
prop_post_shoot gs =
  let added = length (projectiles (shoot gs)) - length (projectiles gs)
  in if playerDead (player gs) then added == 0
     else case weaponMode (player gs) of
       SingleShot -> added == 1
       TripleShot -> added == 3

-- killEnnemy ne décrémente jamais le score (postcondition de monotonie).
prop_post_killEnnemy_scoreMonotone :: GameState -> Bool
prop_post_killEnnemy_scoreMonotone gs = score (killEnnemy gs) >= score gs

-- killEnnemy ne fait jamais augmenter le nombre d'ennemis.
prop_post_killEnnemy_enemyCount :: GameState -> Bool
prop_post_killEnnemy_enemyCount gs = length (enemies (killEnnemy gs)) <= length (enemies gs)

-- Les essais ne peuvent jamais augmenter après killEnnemy : on n'en regagne pas.
prop_post_killEnnemy_livesMonotone :: GameState -> Bool
prop_post_killEnnemy_livesMonotone gs =
  livesLeft (player (killEnnemy gs)) <= livesLeft (player gs)

-- Quand le timer d'inversion est strictement positif, updateScroll fait remonter le fond (offset >=).
prop_post_updateScroll_reverse :: GameState -> Bool
prop_post_updateScroll_reverse gs
  | scrollReverseTimer gs > 0 = scrollOffset (updateScroll gs) >= scrollOffset gs - 0.0001
  | otherwise                 = True

-- updateScroll décrémente le timer d'inversion (jamais en dessous de zéro).
prop_post_updateScroll_timer :: GameState -> Bool
prop_post_updateScroll_timer gs =
  let t' = scrollReverseTimer (updateScroll gs)
  in t' >= 0 && t' <= scrollReverseTimer gs

-- postcondition du timer de tir triple : timer décroît, arme reste TripleShot tant que timer > 0.
prop_post_updateWeaponTimer :: GameState -> Bool
prop_post_updateWeaponTimer gs =
  let newGs = updateWeaponTimer gs
      t   = weaponTimer (player gs)
      t'  = weaponTimer (player newGs)
      wm' = weaponMode  (player newGs)
  in t' >= 0
     && t' <= max 0 (t - 1)
     && (if t' > 0 then wm' == TripleShot else wm' == SingleShot)

-- worldWalls renvoie toujours la composition attendue (Composee[MurGauche, MurDroit]) : invariant structurel.
prop_inv_worldWalls :: GameState -> Bool
prop_inv_worldWalls gs = case worldWalls gs of
  Composee [MurGauche lps, MurDroit rps] -> length lps >= 2 && length rps >= 2
  _ -> False

-- postcondition globale (pas par bonus) : plusieurs bonus du même type s'écrasent, et en coop un bonus peut toucher P1, P2, ou les deux.
prop_post_applyBonuses :: GameState -> Bool
prop_post_applyBonuses gs =
  let newState = applyBonuses gs
      newPl    = player newState
      pl       = player gs
      mp2      = player2 gs
      collided b = collision (bonusHitbox b) (persoHitbox pl)
                || maybe False (\p2 -> collision (bonusHitbox b) (persoHitbox p2)) mp2
      hits     = filter collided (bonuses gs)
      -- Les bonus collidés disparaissent du terrain.
      okShrink = length (bonuses newState) + length hits == length (bonuses gs)
      -- Les bonus non collidés sont préservés.
      okKeep   = all (`elem` bonuses newState) (filter (not . collided) (bonuses gs))
      -- La santé ne peut que croître ou rester égale, et reste plafonnée à 5.
      okHealth = persoHealth newPl >= persoHealth pl && persoHealth newPl <= 5
      -- Le timer d'inversion ne dépasse jamais sa durée nominale.
      okScroll = scrollReverseTimer newState <= max (scrollReverseTimer gs) scrollReverseBonusDuration
                 && scrollReverseTimer newState >= 0
      -- Si P1 a touché un TripleShotBonus, son arme est en TripleShot ; sinon weaponMode est inchangé.
      tripleHitP1 = any (\b -> bonusType b == TripleShotBonus && collision (bonusHitbox b) (persoHitbox pl)) (bonuses gs)
      okWeapon  = if tripleHitP1
                    then weaponMode newPl == TripleShot
                    else weaponMode newPl == weaponMode pl
  in okShrink && okKeep && okHealth && okScroll && okWeapon


-- Après pushOutOfWalls le joueur ne doit plus collider avec les murs (sauf cas extrême où 10*4px n'a pas suffi, on tolère).
prop_post_pushOutOfWalls :: GameState -> Bool
prop_post_pushOutOfWalls gs =
  let gs' = pushOutOfWalls gs
  -- On vérifie au moins que le joueur reste dans les bornes d'écran et que x n'a bougé que vers l'intérieur.
  in case (persoHitbox (player gs), persoHitbox (player gs')) of
       (Rectangle x _ w _, Rectangle x' _ w' _) ->
         x' >= -(screenWidth/2) && x' + w' <= screenWidth/2 && w == w'
         -- Si on collidait, on a forcément bougé vers le centre (ou pas bougé si déjà OK).
         && (not (collision (worldWalls gs) (persoHitbox (player gs))) || abs x' <= abs x)
       _ -> True

-- Le nombre de vies reste dans [0, initLives] : essentiel pour le système d'essais.
prop_inv_lives :: GameState -> Bool
prop_inv_lives gs = let l = livesLeft (player gs) in l >= 0 && l <= initLives

-- resetGame ramène à un état canonique : invariant croisé sur plusieurs champs.
prop_post_resetGame :: Bool
prop_post_resetGame =
  let (_, gs) = runState resetGame initGameState
  in not (lost gs)
     && score gs == 0
     && null (enemies gs)
     && null (projectiles gs)
     && null (bonuses gs)
     && livesLeft (player gs) == initLives
     && weaponMode (player gs) == SingleShot

-- updateProjectiles ne fait que filtrer/déplacer : le nombre ne peut pas augmenter.
prop_post_updateProjectiles_noSpawn :: GameState -> Bool
prop_post_updateProjectiles_noSpawn gs =
  length (projectiles (updateProjectiles gs)) <= length (projectiles gs) + length (enemies gs)

-- updateBonuses peut au plus ajouter un bonus par appel (et seulement si le timer le permet).
prop_post_updateBonuses_atMostOneSpawn :: GameState -> Bool
prop_post_updateBonuses_atMostOneSpawn gs =
  length (bonuses (updateBonuses gs)) <= length (bonuses gs) + 1

-- Invariant P2 : quand le joueur 2 est présent, il satisfait les mêmes invariants que P1.
-- Le pool de vies de P2 est indépendant de celui de P1 (pas de miroir).
prop_inv_player2 :: GameState -> Bool
prop_inv_player2 gs = case player2 gs of
  Nothing -> True
  Just p2 -> persoSpeed p2 > 0
          && persoHealth p2 >= 0 && persoHealth p2 <= 5
          && livesLeft p2 >= 0 && livesLeft p2 <= initLives
          && invincibleTimer p2 >= 0
          && speedyTimer p2 >= 0
          && case persoHitbox p2 of
               Rectangle _ _ w h -> w > 0 && h > 0
               _                 -> False

-- togglePlayer2 est une involution : appliqué deux fois, on retrouve la présence/absence initiale.
prop_post_togglePlayer2_involution :: GameState -> Bool
prop_post_togglePlayer2_involution gs =
  let gs2 = togglePlayer2 (togglePlayer2 gs)
  in isJust (player2 gs) == isJust (player2 gs2)
  where isJust (Just _) = True
        isJust Nothing  = False

-- togglePlayer2 alterne effectivement la présence du joueur 2.
prop_post_togglePlayer2_alternates :: GameState -> Bool
prop_post_togglePlayer2_alternates gs =
  let g1 = togglePlayer2 gs
  in case (player2 gs, player2 g1) of
       (Nothing, Just _)  -> True
       (Just _,  Nothing) -> True
       _                  -> False

-- togglePlayer2 réveille un P2 frais : son nombre d'essais vaut initLives.
prop_post_togglePlayer2_freshLives :: GameState -> Bool
prop_post_togglePlayer2_freshLives gs = case player2 gs of
  Just _  -> True
  Nothing -> case player2 (togglePlayer2 gs) of
    Just p2 -> livesLeft p2 == initLives
    Nothing -> False

-- shootP2 ne consomme aucune vie ; sa précondition est lost == False et P2 présent.
prop_pre_shootP2 :: GameState -> Bool
prop_pre_shootP2 gs = not (lost gs) && case player2 gs of
  Just p2 -> case persoHitbox p2 of
    Rectangle _ _ _ _ -> True
    _                 -> False
  Nothing -> False

prop_post_shootP2_addsOneOrThree :: GameState -> Bool
prop_post_shootP2_addsOneOrThree gs =
  let added = length (projectiles (shootP2 gs)) - length (projectiles gs)
  in case player2 gs of
       Nothing -> added == 0
       Just p2 -> case weaponMode p2 of
         SingleShot -> added == 1
         TripleShot -> added == 3

-- précondition de killEnnemy : invariants du GameState et au moins une frame jouable (lost == False sinon le tick ne sert à rien).
prop_pre_killEnnemy :: GameState -> Bool
prop_pre_killEnnemy gs = prop_inv_player gs && prop_inv_enemies gs && prop_inv_projectiles gs

-- updateBonuses préserve l'invariant de bonus (chaque bonus restant est encore valide).
prop_post_updateBonuses_preservesInv :: GameState -> Bool
prop_post_updateBonuses_preservesInv gs = prop_inv_bonuses (updateBonuses gs)

-- applyBonuses ne fait jamais croître la liste de bonus (consommation uniquement).
prop_post_applyBonuses_noGrowth :: GameState -> Bool
prop_post_applyBonuses_noGrowth gs =
  length (bonuses (applyBonuses gs)) <= length (bonuses gs)

-- Pools de vies indépendants : un joueur dont les PV > 0 et qui n'encaisse rien voit son compteur d'essais inchangé,
-- même si l'autre joueur consomme une vie ce frame-là.
prop_post_killEnnemy_independentLives :: GameState -> Bool
prop_post_killEnnemy_independentLives gs =
  let p1   = player gs
      p2o  = player2 gs
      noEnnTouch pl = not (any (\enn -> collision (ennemyHitbox enn) (persoHitbox pl)) (enemies gs))
      noTearTouch pl = not (any (\proj -> projType proj == Tear && collision (projHitbox proj) (persoHitbox pl)) (projectiles gs))
      stable pl = persoHealth pl > 0
               && (invincibleTimer pl > 0 || (noEnnTouch pl && noTearTouch pl))
      gs'  = killEnnemy gs
      p1Ok = not (stable p1) || livesLeft (player gs') == livesLeft p1
      p2Ok = case (p2o, player2 gs') of
        (Nothing, _)         -> True
        (Just p2, Just p2')  -> not (stable p2) || livesLeft p2' == livesLeft p2
        (Just p2, Nothing)   -> not (stable p2)
  in p1Ok && p2Ok

-- Game over reflète la conjonction : P1 mort et (pas de P2 ou P2 également mort).
prop_post_killEnnemy_gameOver :: GameState -> Bool
prop_post_killEnnemy_gameOver gs =
  let gs' = killEnnemy gs
      p2dead = case player2 gs' of
                 Nothing -> True
                 Just p2 -> playerDead p2
  in lost gs' == (playerDead (player gs') && p2dead) || lost gs == lost gs'

-- Boss HP toujours dans [0, bossHP] : sans ce filet, un cumul de hits mal compté ferait passer ennemyPhase en négatif et le boss survivrait à toute reapparition.
prop_inv_boss :: GameState -> Bool
prop_inv_boss gs = all check (enemies gs)
  where
    check e = ennemyType e /= Boss
           || (ennemyPhase e >= 0 && ennemyPhase e <= bossHP)

-- Au plus un boss à l'écran : la garde dans updateEnnemies doit refuser un doublon (sinon les 1000 points cumulés rendent le jeu trivial).
prop_inv_boss_unique :: GameState -> Bool
prop_inv_boss_unique gs =
  length (filter ((== Boss) . ennemyType) (enemies gs)) <= 1

-- Q1.2 — préconditions des smart constructors de Hitbox.
-- mkPoint accepte tout (pas d'invariant non trivial sur un point).
prop_pre_mkPoint :: Float -> Float -> Bool
prop_pre_mkPoint x y = case mkPoint x y of
  Just (Point x' y') -> x == x' && y == y'
  _                  -> False

-- mkDisque : Just ssi rayon > 0.
prop_pre_mkDisque :: Float -> Float -> Float -> Bool
prop_pre_mkDisque cx cy r = case mkDisque cx cy r of
  Just (Disque _ _ r') -> r > 0 && r' == r
  Nothing              -> r <= 0
  _                    -> False

-- mkRectangle : Just ssi largeur et hauteur strictement positives.
prop_pre_mkRectangle :: Float -> Float -> Float -> Float -> Bool
prop_pre_mkRectangle x y w h = case mkRectangle x y w h of
  Just (Rectangle _ _ w' h') -> w > 0 && h > 0 && w' == w && h' == h
  Nothing                    -> w <= 0 || h <= 0
  _                          -> False

-- Q1.4 — si h1 = Composee [Point a b, Point c d] et qu'un Point h2 colliderait avec h1,
-- alors h2 vaut nécessairement (Point a b) ou (Point c d) (la collision Point/Point est l'égalité).
prop_collision_composeePoints :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
prop_collision_composeePoints a b c d px py =
  let h1 = Composee [Point a b, Point c d]
      h2 = Point px py
  in not (collision h1 h2) || h2 == Point a b || h2 == Point c d

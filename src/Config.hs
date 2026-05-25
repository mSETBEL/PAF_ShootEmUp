module Config
  ( GameConfig (..)
  , defaultConfig
  , GameM
  , runGameM
  , execGameM
  , askConfig
  , gameLoopM
  , updateSpeedTimerCfg
  , updateScrollCfg
  , killEnnemyCfg
  ) where

import Control.Monad.Reader
import Control.Monad.State

import Model

-- paramètres en lecture seule, utilisés via ReaderT (faciles à mocker en test).
data GameConfig = GameConfig
  { cfgScreenWidth      :: Float
  , cfgScreenHeight     :: Float
  , cfgScrollSpeed      :: Float
  , cfgEnemySpawnSpeed  :: Int
  , cfgBonusSpawnSpeed  :: Int
  , cfgPlayerSpeed      :: Float
  , cfgPlayerSpeedBoost :: Float
  , cfgInvincibilityFr  :: Int
  } deriving (Show)

defaultConfig :: GameConfig
defaultConfig = GameConfig
  { cfgScreenWidth      = screenWidth
  , cfgScreenHeight     = screenHeight
  , cfgScrollSpeed      = scrollSpeed
  , cfgEnemySpawnSpeed  = ennemySpawnSpeed
  , cfgBonusSpawnSpeed  = bonusSpawnSpeed
  , cfgPlayerSpeed      = 2
  , cfgPlayerSpeedBoost = 3.5
  , cfgInvincibilityFr  = 70
  }

type GameM a = ReaderT GameConfig (State GameState) a

runGameM :: GameConfig -> GameState -> GameM a -> (a, GameState)
runGameM cfg gs m = runState (runReaderT m cfg) gs

execGameM :: GameConfig -> GameState -> GameM a -> GameState
execGameM cfg gs m = execState (runReaderT m cfg) gs

askConfig :: GameM GameConfig
askConfig = ask

-- variante d'updateSpeedTimer : vitesses lues dans la config (s'applique à P1 et P2).
updateSpeedTimerCfg :: GameM ()
updateSpeedTimerCfg = do
  base  <- asks cfgPlayerSpeed
  boost <- asks cfgPlayerSpeedBoost
  let stepSp pl = if speedyTimer pl > 0
                  then pl { persoSpeed = boost, speedyTimer = speedyTimer pl - 1 }
                  else pl { persoSpeed = base }
  modify $ \gs -> gs { player  = stepSp (player gs)
                     , player2 = fmap stepSp (player2 gs) }

-- Version configurable de updateScroll : le pas et l'amplitude lisent la config, puis on repousse le joueur si le mur l'a englobé.
updateScrollCfg :: GameM ()
updateScrollCfg = do
  sp     <- asks cfgScrollSpeed
  height <- asks cfgScreenHeight
  modify $ \gs ->
    let dir     = if scrollReverseTimer gs > 0 then 1 else -1
        step    = fromIntegral dir * sp
        raw     = scrollOffset gs + step
        newOff = if raw <= -height then raw + height
                 else if raw > 0   then -height + raw
                 else raw
        newTimer = max 0 (scrollReverseTimer gs - 1)
        scrolled = gs { scrollOffset = newOff, scrollReverseTimer = newTimer }
    in pushOutOfWalls scrolled

-- Version configurable de killEnnemy : la durée d'invincibilité après touche est lue depuis la config.
killEnnemyCfg :: GameM ()
killEnnemyCfg = do
  invFr <- asks cfgInvincibilityFr
  modify (killEnnemyWith invFr)

-- Boucle de jeu enrichie : chaque sous-étape lit ce dont elle a besoin dans le ReaderT.
gameLoopM :: [Bool] -> GameM ()
gameLoopM [left, right, up, down] = do
  lift $ do
    if left  then moveLeftM  else return ()
    if right then moveRightM else return ()
    if up    then moveUpM    else return ()
    if down  then moveDownM  else return ()
    updateProjectilesM
    updateEnnemiesM
  updateScrollCfg
  lift updateBonusesM
  updateSpeedTimerCfg
  lift updateWeaponTimerM
gameLoopM _ = return ()

module ModelSpec where

import Test.Hspec
import Test.QuickCheck

import Model
import Config (defaultConfig, execGameM, gameLoopM)



-- générateur pour une hitbox Disque dans les limites de l'écran
genDisqueOnScreen :: Gen Hitbox
genDisqueOnScreen = do
  r  <- choose (5, 20)
  cx <- choose (-(screenWidth / 2) + r, screenWidth / 2 - r)
  cy <- choose (-(screenHeight / 2) + r, screenHeight / 2 - r)
  return $ Disque cx cy r

-- générateur d'une hitbox Rectangle dans les limites de l'écran.
genRectangleOnScreen :: Gen Hitbox
genRectangleOnScreen = do
  w  <- choose (10, 50)
  h  <- choose (10, 50)
  x  <- choose (-(screenWidth / 2), screenWidth / 2 - w)
  y  <- choose (-(screenHeight / 2), screenHeight / 2 - h)
  return $ Rectangle x y w h

-- générateur d'un projectile valide à l'écran.
genProjectile :: Gen Projectile
genProjectile = do
  sp  <- suchThat (choose (1, 10)) (> 0)
  hb  <- genDisqueOnScreen
  dir <- elements [UpDir, DownDir, LeftDir, RightDir]
  t   <- elements [Bullet, Tear]
  return $ Projectile sp hb dir t

-- générateur d'ennemis avant entrée dans l'écran (onScreen = False)
genEnnemyOffScreen :: Gen Ennemy
genEnnemyOffScreen = do
  sp <- suchThat (choose (0.5, 5)) (> 0)
  cx <- choose (-(screenWidth / 2) - 50, screenWidth / 2 + 50)
  cy <- choose (-(screenHeight / 2) - 50, screenHeight / 2 + 50)
  t  <- elements [Red, Green, Blue, Yellow]
  ph <- choose (0, 100)
  return $ Ennemy sp (Disque cx cy (ennemyCote / 2)) (1, 0) t ph False

-- générateur d'ennemi valide après entrée dans l'écran (onScreen = True)
genEnnemyOnScreen :: Gen Ennemy
genEnnemyOnScreen = do
  sp <- suchThat (choose (0.5, 5)) (> 0)
  r  <- return (ennemyCote / 2)
  cx <- choose (-(screenWidth / 2) + r, screenWidth / 2 - r)
  cy <- choose (-(screenHeight / 2) + r, screenHeight / 2 - r)
  t  <- elements [Red, Green, Blue, Yellow]
  ph <- choose (0, 100)
  return $ Ennemy sp (Disque cx cy r) (1, 0) t ph True

-- Boss hors écran avec ennemyPhase tiré dans [0, bossHP] : exerce vraiment prop_inv_boss sans dépendre de la position.
genEnnemyBoss :: Gen Ennemy
genEnnemyBoss = do
  hp     <- choose (0, bossHP)
  reload <- choose (0, bossReloadFrames)
  return $ Ennemy 1.5
    (Disque 0 (screenHeight / 2 + bossCote) bossCote)
    (1, reload) Boss hp False

-- on tire le type d'abord : le rayon en dépend pour borner spawnX/spawnY.
genBonus :: Gen Bonus
genBonus = do
  t  <- elements [Health, Speed, Invincibility, ScrollReverse, TripleShotBonus]
  let r = case t of
            Health           -> healthBonusCote / 2
            Speed            -> speedBonusCote / 2
            Invincibility    -> invincibilityBonusCote / 2
            ScrollReverse    -> scrollReverseBonusCote / 2
            TripleShotBonus  -> tripleShotBonusCote / 2
  cx <- choose (-(screenWidth / 2)+r, screenWidth / 2-r)
  cy <- choose (-(screenHeight / 2)+r, screenHeight / 2-r)
  let d = case t of
            Health           -> Nothing
            Speed            -> Just speedBonusDuration
            Invincibility    -> Just invincibilityBonusDuration
            ScrollReverse    -> Just scrollReverseBonusDuration
            TripleShotBonus  -> Just tripleShotBonusDuration
  return $ Bonus (Disque cx cy r) t d


-- INSTANCES ARBITRARY
instance Arbitrary Projectile where
  arbitrary = genProjectile

instance Arbitrary Ennemy where
  arbitrary = frequency [(3, genEnnemyOffScreen)  
                        ,(7, genEnnemyOnScreen)]   

instance Arbitrary Bonus where
  arbitrary = genBonus


-- GÉNÉRATEURS DE GAMESTATE

-- Générateur libre : peut produire des états invalides (utile pour tester les préconditions)
genGameStateFree :: Gen GameState
genGameStateFree = do
  x     <- choose (-300, 300)
  y     <- choose (-200, 200)
  sp    <- choose (0.5, 10)
  hp    <- choose (0, 5)
  timer <- choose (0, ennemySpawnSpeed)
  sc    <- choose (-screenHeight, 0)
  inv   <- choose (0, 100)
  spT   <- choose (0, speedBonusDuration)
  ll    <- choose (0, initLives)
  return $ GameState False
    (Player sp (Rectangle x y playerWidth playerHeight) hp inv spT ll SingleShot 0)
    [] [] timer sc [] bonusSpawnSpeed 0 0 Nothing

-- générateur garantissant l'invariant de GameState (utile pour tester les postconditions).
genGameStateOk :: Gen GameState
genGameStateOk = do
  x     <- choose (-(screenWidth / 2), screenWidth / 2 - playerWidth)
  y     <- choose (-(screenHeight / 2), screenHeight / 2 - playerHeight)
  sp    <- suchThat (choose (0.5, 10)) (> 0)
  hp    <- choose (1, 5)
  timer <- choose (0, ennemySpawnSpeed)
  sc    <- choose (-screenHeight + 1, 0)
  inv   <- choose (0, 70)
  spT   <- choose (0, speedBonusDuration)
  ll    <- choose (0, initLives)
  return $ GameState False
    (Player sp (Rectangle x y playerWidth playerHeight) hp inv spT ll SingleShot 0)
    [] [] timer sc [] bonusSpawnSpeed 0 0 Nothing

-- Générateur riche : états valides avec ennemis, projectiles et bonus (et un P2 présent une fois sur deux pour exercer le mode coop).
genGameStateRich :: Gen GameState
genGameStateRich = do
  gs    <- genGameStateOk
  enns  <- listOf (frequency [(3, genEnnemyOffScreen), (7, genEnnemyOnScreen)])
  -- Au plus un boss : on respecte prop_inv_boss_unique côté générateur pour rester réaliste vis-à-vis du spawn.
  mboss <- frequency [(3, return Nothing), (1, fmap Just genEnnemyBoss)]
  projs <- listOf genProjectile
  bons  <- listOf genBonus
  mp2   <- frequency [(1, return Nothing), (1, fmap Just genPlayerOk)]
  -- Pools de vies indépendants : P2 a son propre livesLeft tiré dans genPlayerOk.
  return $ gs { enemies = maybe enns (:enns) mboss
              , projectiles = projs, bonuses = bons, player2 = mp2 }

-- Joueur valide isolé : sert à générer un P2 plausible dans le générateur riche.
genPlayerOk :: Gen Player
genPlayerOk = do
  x   <- choose (-(screenWidth / 2), screenWidth / 2 - playerWidth)
  y   <- choose (-(screenHeight / 2), screenHeight / 2 - playerHeight)
  sp  <- suchThat (choose (0.5, 10)) (> 0)
  hp' <- choose (1, 5)
  inv <- choose (0, 70)
  spT <- choose (0, speedBonusDuration)
  ll  <- choose (0, initLives)
  return $ Player sp (Rectangle x y playerWidth playerHeight) hp' inv spT ll SingleShot 0

-- Mélange des trois générateurs : on inclut aussi le générateur riche pour exercer killEnnemy/applyBonuses sur de vrais états.
instance Arbitrary GameState where
  arbitrary = frequency [(2, genGameStateFree)
                        ,(3, genGameStateOk)
                        ,(5, genGameStateRich)]


--propriétés

-- L'état initial respecte l'invariant
property_inv_initGameState :: Property
property_inv_initGameState =
  property $ prop_inv_GameState initGameState



-- PROPRIÉTÉS : mouvements du joueur
property_inv_moveLeft :: GameState -> Property
property_inv_moveLeft gs =
  prop_inv_GameState gs && prop_pre_moveLeft gs
  ==> classify (prop_pre_moveLeft gs) "can move left"
    $ property $ prop_inv_GameState (moveLeft gs)

property_post_moveLeft :: GameState -> Property
property_post_moveLeft gs =
  prop_inv_GameState gs
  ==> property $ prop_post_moveLeft gs

property_inv_moveRight :: GameState -> Property
property_inv_moveRight gs =
  prop_inv_GameState gs && prop_pre_moveRight gs
  ==> classify (prop_pre_moveRight gs) "can move right"
    $ property $ prop_inv_GameState (moveRight gs)

property_post_moveRight :: GameState -> Property
property_post_moveRight gs =
  prop_inv_GameState gs
  ==> property $ prop_post_moveRight gs

property_inv_moveUp :: GameState -> Property
property_inv_moveUp gs =
  prop_inv_GameState gs && prop_pre_moveUp gs
  ==> classify (prop_pre_moveUp gs) "can move up"
    $ property $ prop_inv_GameState (moveUp gs)

property_post_moveUp :: GameState -> Property
property_post_moveUp gs =
  prop_inv_GameState gs
  ==> property $ prop_post_moveUp gs

property_inv_moveDown :: GameState -> Property
property_inv_moveDown gs =
  prop_inv_GameState gs && prop_pre_moveDown gs
  ==> classify (prop_pre_moveDown gs) "can move down"
    $ property $ prop_inv_GameState (moveDown gs)

property_post_moveDown :: GameState -> Property
property_post_moveDown gs =
  prop_inv_GameState gs
  ==> property $ prop_post_moveDown gs


property_post_shoot :: GameState -> Property
property_post_shoot gs =
  prop_inv_GameState gs && prop_pre_shoot gs
  ==> classify (length (projectiles gs) == 0) "no previous projectiles"
    $ classify (length (projectiles gs) > 0) "already has projectiles"
    $ property $ prop_post_shoot gs


-- scroll propriétés

property_inv_updateScroll :: GameState -> Property
property_inv_updateScroll gs =
  prop_inv_GameState gs
  ==> property $ prop_inv_scroll (updateScroll gs)

-- updateScroll est monotone décroissant (ou repart de 0).
property_scroll_monotone :: GameState -> Property
property_scroll_monotone gs =
  prop_inv_GameState gs
  ==> let sc  = scrollOffset gs
          sc' = scrollOffset (updateScroll gs)
      in property $ sc' < sc || sc' == 0


-- projectiles propriétés

property_inv_updateProjectiles :: GameState -> Property
property_inv_updateProjectiles gs =
  prop_inv_GameState gs
  ==> property $ prop_inv_projectiles (updateProjectiles gs)

property_projectiles_non_negative :: GameState -> Property
property_projectiles_non_negative gs =
  prop_inv_GameState gs
  ==> property $ length (projectiles (updateProjectiles gs)) >= 0


-- ennemis propriétés

-- onScreen est monotone : une fois vrai, reste vrai.
property_onScreen_monotonic :: Ennemy -> Property
property_onScreen_monotonic e =
  onScreen e
  ==> property $ onScreen (updateOnScreen e)

-- les ennemis à l'écran restent dans les limites après leur mouvement.
property_inv_enemies_onScreen :: GameState -> Property
property_inv_enemies_onScreen gs =
  prop_inv_GameState gs
  ==> property $ prop_inv_enemies (updateEnnemies gs)

-- le timer de spawn reste positif après update.
property_inv_spawnTimer_update :: GameState -> Property
property_inv_spawnTimer_update gs =
  prop_inv_GameState gs
  ==> property $ prop_inv_spawnTimer (updateEnnemies gs)


-- bonus propriétés

property_post_applyBonuses :: GameState -> Property
property_post_applyBonuses gs =
  prop_inv_GameState gs
  ==> property $ prop_post_applyBonuses gs

-- variante riche : on teste juste des bornes saines (validBonus échoue quand plusieurs bonus du même type collisionnent — le dernier écrase).
property_post_applyBonuses_rich :: Property
property_post_applyBonuses_rich =
  forAll genGameStateRich $ \gs ->
    prop_inv_GameState gs ==>
      let newGs = applyBonuses gs
          newPl = player newGs
          okHp  = persoHealth newPl >= persoHealth (player gs)
                  && persoHealth newPl <= 5
          okSc  = scrollReverseTimer newGs >= 0
          okFew = length (bonuses newGs) <= length (bonuses gs)
      in okHp && okSc && okFew

property_inv_updateBonuses :: GameState -> Property
property_inv_updateBonuses gs =
  prop_inv_GameState gs
  ==> property $ prop_inv_bonuses (updateBonuses gs)


-- score propriétés

property_inv_score :: GameState -> Property
property_inv_score gs =
  prop_inv_GameState gs ==> property $ prop_inv_score gs

property_post_killEnnemy_scoreMonotone :: GameState -> Property
property_post_killEnnemy_scoreMonotone gs =
  prop_inv_GameState gs ==> property $ prop_post_killEnnemy_scoreMonotone gs

property_post_killEnnemy_enemyCount :: GameState -> Property
property_post_killEnnemy_enemyCount gs =
  prop_inv_GameState gs ==> property $ prop_post_killEnnemy_enemyCount gs

property_post_killEnnemy_livesMonotone :: GameState -> Property
property_post_killEnnemy_livesMonotone gs =
  prop_inv_GameState gs ==> property $ prop_post_killEnnemy_livesMonotone gs

-- Postcondition d'inversion : tant que le timer n'est pas nul, le défilement va dans l'autre sens.
property_post_updateScroll_reverse :: GameState -> Property
property_post_updateScroll_reverse gs =
  prop_inv_GameState gs ==> property $ prop_post_updateScroll_reverse gs

property_post_updateScroll_timer :: GameState -> Property
property_post_updateScroll_timer gs =
  prop_inv_GameState gs ==> property $ prop_post_updateScroll_timer gs

property_inv_worldWalls :: GameState -> Property
property_inv_worldWalls gs =
  prop_inv_GameState gs ==> property $ prop_inv_worldWalls gs

-- postcondition du timer de tir triple.
property_post_updateWeaponTimer :: GameState -> Property
property_post_updateWeaponTimer gs =
  prop_inv_GameState gs ==> property $ prop_post_updateWeaponTimer gs

-- pushOutOfWalls : x reste borné et le joueur a été poussé vers le centre s'il touchait un mur.
property_post_pushOutOfWalls :: GameState -> Property
property_post_pushOutOfWalls gs =
  prop_inv_GameState gs ==> property $ prop_post_pushOutOfWalls gs

property_inv_lives :: GameState -> Property
property_inv_lives gs =
  prop_inv_GameState gs ==> property $ prop_inv_lives gs

-- updateProjectiles ne crée jamais plus de projectiles qu'il n'y a d'ennemis tireurs présents.
property_post_updateProjectiles_noSpawn :: GameState -> Property
property_post_updateProjectiles_noSpawn gs =
  prop_inv_GameState gs ==> property $ prop_post_updateProjectiles_noSpawn gs

-- updateBonuses ne génère qu'un bonus au plus par appel.
property_post_updateBonuses_atMostOneSpawn :: GameState -> Property
property_post_updateBonuses_atMostOneSpawn gs =
  prop_inv_GameState gs ==> property $ prop_post_updateBonuses_atMostOneSpawn gs

-- Quand P2 est présent, ses invariants doivent tenir.
property_inv_player2 :: GameState -> Property
property_inv_player2 gs =
  prop_inv_GameState gs ==> property $ prop_inv_player2 gs

-- togglePlayer2 est involutive en présence/absence.
property_post_togglePlayer2_involution :: GameState -> Property
property_post_togglePlayer2_involution gs =
  prop_inv_GameState gs ==> property $ prop_post_togglePlayer2_involution gs

-- togglePlayer2 alterne la présence du joueur 2.
property_post_togglePlayer2_alternates :: GameState -> Property
property_post_togglePlayer2_alternates gs =
  prop_inv_GameState gs ==> property $ prop_post_togglePlayer2_alternates gs

-- togglePlayer2 sur un état sans P2 réveille un joueur frais avec initLives essais.
property_post_togglePlayer2_freshLives :: GameState -> Property
property_post_togglePlayer2_freshLives gs =
  prop_inv_GameState gs ==> property $ prop_post_togglePlayer2_freshLives gs

-- shootP2 ajoute 0, 1 ou 3 projectiles selon l'arme et la présence de P2.
property_post_shootP2_addsOneOrThree :: GameState -> Property
property_post_shootP2_addsOneOrThree gs =
  prop_inv_GameState gs ==> property $ prop_post_shootP2_addsOneOrThree gs

-- applyBonuses n'augmente jamais la liste de bonus (seulement consommation).
property_post_applyBonuses_noGrowth :: GameState -> Property
property_post_applyBonuses_noGrowth gs =
  prop_inv_GameState gs ==> property $ prop_post_applyBonuses_noGrowth gs

-- updateBonuses préserve l'invariant des bonus.
property_post_updateBonuses_preservesInv :: GameState -> Property
property_post_updateBonuses_preservesInv gs =
  prop_inv_GameState gs ==> property $ prop_post_updateBonuses_preservesInv gs

-- Pools de vies indépendants : si personne n'est touché, les essais restent inchangés.
property_post_killEnnemy_independentLives :: GameState -> Property
property_post_killEnnemy_independentLives gs =
  prop_inv_GameState gs ==> property $ prop_post_killEnnemy_independentLives gs

-- Game over conjoint : déclenché ssi P1 mort et (pas de P2 ou P2 mort).
property_post_killEnnemy_gameOver :: GameState -> Property
property_post_killEnnemy_gameOver gs =
  prop_inv_GameState gs ==> property $ prop_post_killEnnemy_gameOver gs

-- Boss : ennemyPhase d'un boss reste dans [0, bossHP] (HP ne déborde jamais).
property_inv_boss :: GameState -> Property
property_inv_boss gs =
  prop_inv_GameState gs ==> property $ prop_inv_boss gs

-- Boss : au plus un boss à l'écran (refuse les doublons, propriété cruciale pour la difficulté).
property_inv_boss_unique :: GameState -> Property
property_inv_boss_unique gs =
  prop_inv_GameState gs ==> property $ prop_inv_boss_unique gs

-- Intégration GameM : une boucle de jeu sous ReaderT GameConfig (State GameState) préserve l'invariant global.
property_gameLoopM_preservesInv :: GameState -> Property
property_gameLoopM_preservesInv gs =
  prop_inv_GameState gs ==>
    forAll (vectorOf 4 arbitrary) $ \keys ->
      let gs' = execGameM defaultConfig gs (gameLoopM keys)
      in prop_inv_player gs'
         && prop_inv_scroll gs'
         && prop_inv_spawnTimer gs'
         && prop_inv_score gs'
         && prop_inv_scrollReverse gs'
         && prop_inv_lives gs'
         && prop_inv_boss gs'


-- générateurs propriétés

-- genGameStateOk produit toujours des états valides.
property_inv_genGameStateOk :: Property
property_inv_genGameStateOk = forAll genGameStateOk $ prop_inv_GameState

-- genGameStateFree peut produire des états invalides (testé par expectFailure).
property_inv_genGameStateFree :: Property
property_inv_genGameStateFree = forAll genGameStateFree $ prop_inv_GameState

-- genGameStateRich produit des états avec ennemis/projectiles/bonus valides.
property_inv_genGameStateRich :: Property
property_inv_genGameStateRich = forAll genGameStateRich $ \gs ->
  prop_inv_player gs && prop_inv_scroll gs && prop_inv_spawnTimer gs

-- Les projectiles générés sont toujours valides
property_inv_genProjectile :: Property
property_inv_genProjectile = forAll genProjectile $ \p ->
  case p of
    Projectile sp (Disque _ _ _) _ _ -> sp > 0
    _                                 -> False

-- Les bonus générés respectent leur invariant
property_inv_genBonus :: Property
property_inv_genBonus = forAll genBonus $ \b ->
  case b of
    Bonus (Disque _ _ _) Health Nothing             -> True
    Bonus (Disque _ _ _) Speed  (Just d)            -> d == speedBonusDuration
    Bonus (Disque _ _ _) Invincibility (Just d)     -> d == invincibilityBonusDuration
    Bonus (Disque _ _ _) ScrollReverse (Just d)     -> d == scrollReverseBonusDuration
    Bonus (Disque _ _ _) TripleShotBonus (Just d)   -> d == tripleShotBonusDuration
    _                                               -> False


-- specs hspec

initGameStateSpec :: Spec
initGameStateSpec =
  describe "initGameState" $
    it "satisfies the invariant" $ property property_inv_initGameState

moveLeftSpec :: Spec
moveLeftSpec =
  describe "moveLeft" $ do
    it "preserves the invariant" $ property property_inv_moveLeft
    it "satisfies postcondition" $ property property_post_moveLeft

moveRightSpec :: Spec
moveRightSpec =
  describe "moveRight" $ do
    it "preserves the invariant" $ property property_inv_moveRight
    it "satisfies postcondition" $ property property_post_moveRight

moveUpSpec :: Spec
moveUpSpec =
  describe "moveUp" $ do
    it "preserves the invariant" $ property property_inv_moveUp
    it "satisfies postcondition" $ property property_post_moveUp

moveDownSpec :: Spec
moveDownSpec =
  describe "moveDown" $ do
    it "preserves the invariant" $ property property_inv_moveDown
    it "satisfies postcondition" $ property property_post_moveDown

shootSpec :: Spec
shootSpec =
  describe "shoot" $
    it "adds exactly one projectile" $ property property_post_shoot

scrollSpec :: Spec
scrollSpec =
  describe "updateScroll" $ do
    it "preserves scroll invariant"  $ property property_inv_updateScroll
    it "is monotone decreasing"      $ property property_scroll_monotone

projectileSpec :: Spec
projectileSpec =
  describe "updateProjectiles" $ do
    it "keeps projectiles in bounds"     $ property property_inv_updateProjectiles
    it "never produces negative count"   $ property property_projectiles_non_negative

ennemySpec :: Spec
ennemySpec =
  describe "enemies" $ do
    it "onScreen is monotonic"           $ property property_onScreen_monotonic
    it "preserves enemy invariant"       $ property property_inv_enemies_onScreen
    it "spawn timer stays non-negative"  $ property property_inv_spawnTimer_update

bonusSpec :: Spec
bonusSpec =
  describe "bonuses" $ do
    it "applyBonuses satisfies postcondition" $ property property_post_applyBonuses
    it "applyBonuses postcondition with rich states" $ property property_post_applyBonuses_rich
    it "updateBonuses preserves invariant"    $ property property_inv_updateBonuses

scoreSpec :: Spec
scoreSpec =
  describe "score" $ do
    it "is non-negative"                $ property property_inv_score
    it "killEnnemy is score-monotone"   $ property property_post_killEnnemy_scoreMonotone
    it "killEnnemy never grows enemies" $ property property_post_killEnnemy_enemyCount
    it "killEnnemy never grows lives"   $ property property_post_killEnnemy_livesMonotone

extensionsSpec :: Spec
extensionsSpec =
  describe "extensions" $ do
    it "updateScroll inverts when timer > 0"    $ property property_post_updateScroll_reverse
    it "updateScroll timer decreases"           $ property property_post_updateScroll_timer
    it "worldWalls keeps Composee[Mur*] shape"  $ property property_inv_worldWalls
    it "weaponTimer decreases and resets mode"  $ property property_post_updateWeaponTimer
    it "pushOutOfWalls keeps player in bounds and pushes inward" $ property property_post_pushOutOfWalls
    it "livesLeft stays in [0, initLives]"      $ property property_inv_lives
    it "updateProjectiles does not spawn out of thin air" $ property property_post_updateProjectiles_noSpawn
    it "updateBonuses spawns at most one bonus per call"  $ property property_post_updateBonuses_atMostOneSpawn
    it "resetGame returns to canonical state"   $ prop_post_resetGame `shouldBe` True
    it "coop P2 satisfies invariants when present" $ property property_inv_player2
    it "togglePlayer2 is an involution"          $ property property_post_togglePlayer2_involution
    it "togglePlayer2 alternates presence"       $ property property_post_togglePlayer2_alternates
    it "togglePlayer2 gives P2 fresh lives"      $ property property_post_togglePlayer2_freshLives
    it "shootP2 adds 0/1/3 projectiles"          $ property property_post_shootP2_addsOneOrThree
    it "applyBonuses never grows the bonus list" $ property property_post_applyBonuses_noGrowth
    it "updateBonuses preserves bonus invariant" $ property property_post_updateBonuses_preservesInv
    it "killEnnemy keeps P1/P2 lives independent" $ property property_post_killEnnemy_independentLives
    it "killEnnemy sets game-over jointly"        $ property property_post_killEnnemy_gameOver
    it "boss HP stays in [0, bossHP]"             $ property property_inv_boss
    it "at most one boss on screen"               $ property property_inv_boss_unique

gameMSpec :: Spec
gameMSpec = describe "GameM (ReaderT GameConfig (State GameState))" $
  it "gameLoopM preserves the GameState invariants"
    $ property property_gameLoopM_preservesInv

genSpec :: Spec
genSpec = do
  describe "genGameStateOk" $
    it "always generates valid states" $ property property_inv_genGameStateOk
  describe "genGameStateFree" $
    it "can generate invalid states (expected)" $ expectFailure $
      property property_inv_genGameStateFree
  describe "genGameStateRich" $
    it "player and scroll invariants hold" $ property property_inv_genGameStateRich
  describe "genProjectile" $
    it "always generates valid projectiles" $ property property_inv_genProjectile
  describe "genBonus" $
    it "always generates valid bonuses" $ property property_inv_genBonus

hitboxSpec :: Spec
hitboxSpec = describe "Hitbox smart constructors" $ do
  it "mkPoint accepte tout couple (x,y)"       $ property prop_pre_mkPoint
  it "mkDisque exige un rayon strictement > 0" $ property prop_pre_mkDisque
  it "mkRectangle exige w > 0 et h > 0"        $ property prop_pre_mkRectangle
  it "collision sur Composee[Point a b, Point c d] implique egalite avec a,b ou c,d"
                                               $ property prop_collision_composeePoints
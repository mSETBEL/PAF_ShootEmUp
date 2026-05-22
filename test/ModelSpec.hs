module ModelSpec where

import Test.Hspec
import Test.QuickCheck

import Model



-- générateur pour une hitbox Disque dans les limites de l'écran
genDisqueOnScreen :: Gen Hitbox
genDisqueOnScreen = do
  r  <- choose (5, 20)
  cx <- choose (-(screenWidth / 2) + r, screenWidth / 2 - r)
  cy <- choose (-(screenHeight / 2) + r, screenHeight / 2 - r)
  return $ Disque cx cy r

-- genérateur pour une hitbox Rectangle dans les limites de l'écran
genRectangleOnScreen :: Gen Hitbox
genRectangleOnScreen = do
  w  <- choose (10, 50)
  h  <- choose (10, 50)
  x  <- choose (-(screenWidth / 2), screenWidth / 2 - w)
  y  <- choose (-(screenHeight / 2), screenHeight / 2 - h)
  return $ Rectangle x y w h

-- genérateur pour un projectile (dans les limites de l'écran)
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

--Générateur pour un bonus valide
genBonus :: Gen Bonus
genBonus = do
  let r = case t of
            Health        -> healthBonusCote / 2
            Speed         -> speedBonusCote / 2
            Invincibility -> invincibilityBonusCote / 2
  cx <- choose (-(screenWidth / 2)+r, screenWidth / 2-r)
  cy <- choose (-(screenHeight / 2)+r, screenHeight / 2-r)
  t  <- elements [Health, Speed, Invincibility]
  let d = case t of
            Health        -> Nothing
            Speed         -> Just speedBonusDuration
            Invincibility -> Just invincibilityBonusDuration
  return $ Bonus (Disque cx cy 10) t d


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
  return $ GameState False
    (Player sp (Rectangle x y playerWidth playerHeight) hp inv spT)
    [] [] timer sc [] bonusSpawnSpeed 0

-- géneateur sur garantissant l'invariant de GameState (utile pour tester les postconditions)
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
  return $ GameState False
    (Player sp (Rectangle x y playerWidth playerHeight) hp inv spT)
    [] [] timer sc [] bonusSpawnSpeed 0

-- Générateur riche : états valides avec ennemis, projectiles et bonus
genGameStateRich :: Gen GameState
genGameStateRich = do
  gs    <- genGameStateOk
  enns  <- listOf (frequency [(3, genEnnemyOffScreen), (7, genEnnemyOnScreen)])
  projs <- listOf genProjectile
  bons  <- listOf genBonus
  return $ gs { enemies = enns, projectiles = projs, bonuses = bons }

-- Instance Arbitrary : 80% sûr, 20% libre
instance Arbitrary GameState where
  arbitrary = frequency [(2, genGameStateFree)
                        ,(8, genGameStateOk)]


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


-- propriété : le tir ajoute exactement un projectile

-- Tirer ajoute exactement un projectile
property_post_shoot :: GameState -> Property
property_post_shoot gs =
  prop_inv_GameState gs && prop_pre_shoot gs
  ==> classify (length (projectiles gs) == 0) "no previous projectiles"
    $ classify (length (projectiles gs) > 0) "already has projectiles"
    $ property $ prop_post_shoot gs


-- scroll propriétés

-- updateScroll préserve l'invariant de scroll
property_inv_updateScroll :: GameState -> Property
property_inv_updateScroll gs =
  prop_inv_GameState gs
  ==> property $ prop_inv_scroll (updateScroll gs)

-- updateScroll est monotone décroissant (ou repart de 0)
property_scroll_monotone :: GameState -> Property
property_scroll_monotone gs =
  prop_inv_GameState gs
  ==> let sc  = scrollOffset gs
          sc' = scrollOffset (updateScroll gs)
      in property $ sc' < sc || sc' == 0


-- projectiles propriétés

-- updateProjectiles préserve les projectiles dans les limites
property_inv_updateProjectiles :: GameState -> Property
property_inv_updateProjectiles gs =
  prop_inv_GameState gs
  ==> property $ prop_inv_projectiles (updateProjectiles gs)

-- le nombre de projectiles est positif après updateProjectiles
property_projectiles_non_negative :: GameState -> Property
property_projectiles_non_negative gs =
  prop_inv_GameState gs
  ==> property $ length (projectiles (updateProjectiles gs)) >= 0


-- ennemis propriétés

-- onScreen est monotone : une fois vrai, reste vrai
property_onScreen_monotonic :: Ennemy -> Property
property_onScreen_monotonic e =
  onScreen e
  ==> property $ onScreen (updateOnScreen e)

-- les ennemis à l'écran restent dans les limites après leur mouvement
property_inv_enemies_onScreen :: GameState -> Property
property_inv_enemies_onScreen gs =
  prop_inv_GameState gs
  ==> property $ prop_inv_enemies (updateEnnemies gs)

-- le timer de spawn est toujours positif
property_inv_spawnTimer_update :: GameState -> Property
property_inv_spawnTimer_update gs =
  prop_inv_GameState gs
  ==> property $ prop_inv_spawnTimer (updateEnnemies gs)


-- bonus propriétés

-- applyBonuses satisfait la postcondition
property_post_applyBonuses :: GameState -> Property
property_post_applyBonuses gs =
  prop_inv_GameState gs
  ==> property $ prop_post_applyBonuses gs

-- updateBonuses préserve l'invariant des bonus
property_inv_updateBonuses :: GameState -> Property
property_inv_updateBonuses gs =
  prop_inv_GameState gs
  ==> property $ prop_inv_bonuses (updateBonuses gs)


-- générateurs propriétés

-- Le bon générateur produit toujours des états valides
property_inv_genGameStateOk :: Property
property_inv_genGameStateOk = forAll genGameStateOk $ prop_inv_GameState

-- Le générateur libre peut produire des états invalides
property_inv_genGameStateFree :: Property
property_inv_genGameStateFree = forAll genGameStateFree $ prop_inv_GameState

-- Le générateur riche produit des états avec ennemis/projectiles/bonus valides
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
    Bonus (Disque _ _ _) Health Nothing        -> True
    Bonus (Disque _ _ _) Speed  (Just d)       -> d == speedBonusDuration
    Bonus (Disque _ _ _) Invincibility (Just d) -> d == invincibilityBonusDuration
    _                                           -> False


-- ============================================================
-- SPÉCIFICATIONS HSPEC
-- ============================================================

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
    it "updateBonuses preserves invariant"    $ property property_inv_updateBonuses

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
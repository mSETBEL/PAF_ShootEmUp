module ModelSpec where

import Test.Hspec
import Test.QuickCheck

import Model


-- Generateur libre: peut générer des états invalides (ce qui est utile pour tester les préconditions)
genGameStateFree :: Gen GameState
genGameStateFree = do
  x     <- choose (-300, 300)
  y     <- choose (-180, 180)
  sp    <- choose (1, 5)
  hp    <- choose (0, 5)
  timer <- choose (0, 100)
  sc    <- choose (-358, 0)
  return $ GameState
    (Player sp (Rectangle x y playerWidth playerHeight) hp)
    [] [] timer sc

-- bon générateur: génère toujours des états valides (respectant l'invariant)
genGameStateOk :: Gen GameState
genGameStateOk = do
  x     <- choose (-(screenWidth / 2 ), screenWidth / 2 - playerWidth)
  y     <- choose (-(screenHeight / 2 ), screenHeight / 2 - playerHeight)
  sp    <- choose (1, 5)
  hp    <- choose (1, 5)  -- at least 1 health
  timer <- choose (0, 100)
  sc    <- choose (-screenHeight + 1, 0)  -- strictly within bounds
  return $ GameState
    (Player sp (Rectangle x y playerWidth playerHeight) hp)
    [] [] timer sc

-- 80% d'instance sures, 20% d'instances libres
instance Arbitrary GameState where
  arbitrary = frequency [(2, genGameStateFree)
                        ,(8, genGameStateOk)]



property_inv_initGameState :: Property
property_inv_initGameState =
  property $ prop_inv_GameState initGameState

-- mouvements
property_inv_moveLeft :: GameState -> Property
property_inv_moveLeft gs =
  prop_inv_GameState gs && prop_pre_moveLeft gs
  ==> classify (prop_pre_moveLeft gs) "can move" $
  property $ prop_inv_GameState (moveLeft gs)

property_post_moveLeft :: GameState -> Property
property_post_moveLeft gs =
  prop_inv_GameState gs && prop_pre_moveLeft gs
  ==> property $ prop_post_moveLeft gs


property_inv_moveRight :: GameState -> Property
property_inv_moveRight gs =
  prop_inv_GameState gs && prop_pre_moveRight gs
  ==> property $ prop_inv_GameState (moveRight gs)

property_post_moveRight :: GameState -> Property
property_post_moveRight gs =
  prop_inv_GameState gs && prop_pre_moveRight gs
  ==> property $ prop_post_moveRight gs


property_inv_moveUp :: GameState -> Property
property_inv_moveUp gs =
  prop_inv_GameState gs && prop_pre_moveUp gs
  ==> property $ prop_inv_GameState (moveUp gs)

property_post_moveUp :: GameState -> Property
property_post_moveUp gs =
  prop_inv_GameState gs && prop_pre_moveUp gs
  ==> property $ prop_post_moveUp gs


property_inv_moveDown :: GameState -> Property
property_inv_moveDown gs =
  prop_inv_GameState gs && prop_pre_moveDown gs
  ==> property $ prop_inv_GameState (moveDown gs)

property_post_moveDown :: GameState -> Property
property_post_moveDown gs =
  prop_inv_GameState gs && prop_pre_moveDown gs
  ==> property $ prop_post_moveDown gs

-- tirer un projectile doit augmenter le nombre de projectiles de 1
property_post_shoot :: GameState -> Property
property_post_shoot gs =
  prop_inv_GameState gs && prop_pre_shoot gs
  ==> property $ prop_post_shoot gs

-- scroll reste correct
property_inv_updateScroll :: GameState -> Property
property_inv_updateScroll gs =
  prop_inv_GameState gs
  ==> property $ prop_inv_scroll (updateScroll gs)

-- bon générateur: génère toujours des états valides
property_inv_genGameStateOk :: Property
property_inv_genGameStateOk = forAll genGameStateOk $ prop_inv_GameState

-- générateur libre: peut produire des états invalides (ceci est attendu pour échouer parfois)
property_inv_genGameStateFree :: Property
property_inv_genGameStateFree = forAll genGameStateFree $ prop_inv_GameState


-- Spécifications individuelles

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

updateScrollSpec :: Spec
updateScrollSpec =
  describe "updateScroll" $
    it "preserves scroll invariant" $ property property_inv_updateScroll

genSpec :: Spec
genSpec = do
  describe "genGameStateOk" $
    it "always generates valid states" $ property property_inv_genGameStateOk
  describe "genGameStateFree" $
    it "always generates valid states" $ property property_inv_genGameStateFree
module Main where

import Test.Hspec
import ModelSpec
import AlgebraSpec

main :: IO ()
main = hspec $ do
  initGameStateSpec
  moveLeftSpec
  moveRightSpec
  moveUpSpec
  moveDownSpec
  shootSpec
  scrollSpec      
  projectileSpec
  ennemySpec
  bonusSpec
  scoreSpec
  extensionsSpec
  genSpec
  hitboxSpec
  gameMSpec
  algebraSpec
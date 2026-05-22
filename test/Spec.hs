module Main where

import Test.Hspec
import ModelSpec

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
  genSpec
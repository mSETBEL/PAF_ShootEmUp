module Score
  ( Score (..)
  , addScore
  , scoreValue
  ) where

-- newtype pour disposer d'un Monoid additif dédié au score, sans collision avec Num Int.
newtype Score = Score { unScore :: Int }
  deriving (Show, Eq, Ord)

instance Semigroup Score where
  Score a <> Score b = Score (a + b)

instance Monoid Score where
  mempty = Score 0

addScore :: Score -> Score -> Score
addScore = (<>)

scoreValue :: Score -> Int
scoreValue = unScore

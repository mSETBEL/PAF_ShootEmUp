module Vec2
  ( Vec2 (..)
  , vzero
  , vadd
  , vsub
  , vscale
  , vnorm
  , vdot
  , vnormalize
  , toTuple
  , fromTuple
  ) where

-- Vecteur 2D paramétré : factorise les couples (Float,Float) éparpillés.
data Vec2 a = Vec2 !a !a
  deriving (Show, Eq)

instance Functor Vec2 where
  fmap f (Vec2 x y) = Vec2 (f x) (f y)

-- Produit composante par composante : permet d'écrire vadd = liftA2 (+).
instance Applicative Vec2 where
  pure x = Vec2 x x
  (Vec2 f g) <*> (Vec2 x y) = Vec2 (f x) (g y)

instance Num a => Semigroup (Vec2 a) where
  (<>) = vadd

instance Num a => Monoid (Vec2 a) where
  mempty = vzero

vzero :: Num a => Vec2 a
vzero = Vec2 0 0

vadd :: Num a => Vec2 a -> Vec2 a -> Vec2 a
vadd u v = (+) <$> u <*> v

vsub :: Num a => Vec2 a -> Vec2 a -> Vec2 a
vsub u v = (-) <$> u <*> v

vscale :: Num a => a -> Vec2 a -> Vec2 a
vscale k = fmap (* k)

vdot :: Num a => Vec2 a -> Vec2 a -> a
vdot (Vec2 x1 y1) (Vec2 x2 y2) = x1 * x2 + y1 * y2

vnorm :: Floating a => Vec2 a -> a
vnorm v = sqrt (vdot v v)

-- Garde-fou contre la division par zéro quand l'ennemi est sur le joueur.
vnormalize :: (Floating a, Eq a) => Vec2 a -> Vec2 a
vnormalize v =
  let n = vnorm v
  in if n == 0 then vzero else fmap (/ n) v

toTuple :: Vec2 a -> (a, a)
toTuple (Vec2 x y) = (x, y)

fromTuple :: (a, a) -> Vec2 a
fromTuple (x, y) = Vec2 x y

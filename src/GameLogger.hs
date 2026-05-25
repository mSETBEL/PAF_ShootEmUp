module GameLogger
  ( Logger (..)
  , tell
  , logged
  ) where

-- Logger pédagogique : on instancie Monad explicitement pour montrer qu'on a compris les lois sans s'appuyer sur Writer de mtl.
-- On reste indépendant du gameplay : utile aussi comme cible des property tests sur les lois (cf. AlgebraSpec).
newtype Logger w a = Logger { runLogger :: (a, [w]) }
  deriving (Eq, Show)

instance Functor (Logger w) where
  fmap f (Logger (a, ws)) = Logger (f a, ws)

instance Applicative (Logger w) where
  pure x = Logger (x, [])
  Logger (f, ws1) <*> Logger (a, ws2) = Logger (f a, ws1 ++ ws2)

-- Monad pur : la concaténation des logs respecte associativité et neutralité grâce à (++) et [].
instance Monad (Logger w) where
  return = pure
  Logger (a, ws1) >>= k =
    let Logger (b, ws2) = k a
    in Logger (b, ws1 ++ ws2)

-- Émet un événement dans le log courant.
tell :: w -> Logger w ()
tell w = Logger ((), [w])

-- Helper de convenance : associe une valeur à un événement unique.
logged :: w -> a -> Logger w a
logged w a = do tell w; pure a

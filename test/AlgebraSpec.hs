module AlgebraSpec (algebraSpec) where

import Test.Hspec
import Test.QuickCheck

import Vec2
import Score
import GameLogger

-- Générateurs Arbitrary pour les types algébriques afin de tester les lois universelles.
instance (Arbitrary a) => Arbitrary (Vec2 a) where
  arbitrary = Vec2 <$> arbitrary <*> arbitrary

instance Arbitrary Score where
  arbitrary = Score . abs <$> arbitrary

-- Logger : on s'autorise n'importe quelle valeur et n'importe quelle liste de logs.
instance (Arbitrary w, Arbitrary a) => Arbitrary (Logger w a) where
  arbitrary = do
    a  <- arbitrary
    ws <- arbitrary
    return (Logger (a, ws))

-- Lois Functor (identité, composition).
prop_functor_identity :: Vec2 Int -> Bool
prop_functor_identity v = fmap id v == v

prop_functor_compose :: Fun Int Int -> Fun Int Int -> Vec2 Int -> Bool
prop_functor_compose (Fun _ f) (Fun _ g) v =
  fmap (f . g) v == (fmap f . fmap g) v

-- Lois Applicative (identité et composition basiques sur pure et <*>).
prop_applicative_identity :: Vec2 Int -> Bool
prop_applicative_identity v = (pure id <*> v) == v

prop_applicative_homomorphism :: Fun Int Int -> Int -> Bool
prop_applicative_homomorphism (Fun _ f) x =
  (pure f <*> (pure x :: Vec2 Int)) == pure (f x)

-- Lois Monoid pour Score : associativité, identité gauche/droite.
prop_score_monoid_assoc :: Score -> Score -> Score -> Bool
prop_score_monoid_assoc a b c = ((a <> b) <> c) == (a <> (b <> c))

prop_score_monoid_left_id :: Score -> Bool
prop_score_monoid_left_id a = (mempty <> a) == a

prop_score_monoid_right_id :: Score -> Bool
prop_score_monoid_right_id a = (a <> mempty) == a

-- Lois Monoid pour Vec2 (addition vectorielle).
prop_vec2_monoid_assoc :: Vec2 Int -> Vec2 Int -> Vec2 Int -> Bool
prop_vec2_monoid_assoc a b c = ((a <> b) <> c) == (a <> (b <> c))

prop_vec2_monoid_left_id :: Vec2 Int -> Bool
prop_vec2_monoid_left_id a = (mempty <> a) == a

prop_vec2_monoid_right_id :: Vec2 Int -> Bool
prop_vec2_monoid_right_id a = (a <> mempty) == a

-- Lois Monad pour Logger : identité gauche, identité droite, associativité.
-- On instancie le log à String (Arbitrary disponible) et la valeur à Int.

prop_logger_left_identity :: Int -> Fun Int (Logger String Int) -> Bool
prop_logger_left_identity x (Fn f) =
  runLogger (pure x >>= f) == runLogger (f x)

prop_logger_right_identity :: Logger String Int -> Bool
prop_logger_right_identity m =
  runLogger (m >>= pure) == runLogger m

prop_logger_associativity
  :: Logger String Int
  -> Fun Int (Logger String Int)
  -> Fun Int (Logger String Int)
  -> Bool
prop_logger_associativity m (Fn f) (Fn g) =
  runLogger ((m >>= f) >>= g) == runLogger (m >>= (\x -> f x >>= g))

-- tell s'ajoute en queue : vérifie qu'on a bien un Writer-comme accumulateur (pas un Last).
prop_logger_tell_appends :: String -> String -> Bool
prop_logger_tell_appends w1 w2 =
  snd (runLogger (tell w1 >> tell w2)) == [w1, w2]

algebraSpec :: Spec
algebraSpec = describe "structures algebriques" $ do
  describe "Vec2 Functor" $ do
    it "identite"   $ property prop_functor_identity
    it "composition" $ property prop_functor_compose
  describe "Vec2 Applicative" $ do
    it "identite"      $ property prop_applicative_identity
    it "homomorphisme" $ property prop_applicative_homomorphism
  describe "Vec2 Monoid" $ do
    it "associativite"  $ property prop_vec2_monoid_assoc
    it "identite gauche" $ property prop_vec2_monoid_left_id
    it "identite droite" $ property prop_vec2_monoid_right_id
  describe "Score Monoid" $ do
    it "associativite"   $ property prop_score_monoid_assoc
    it "identite gauche" $ property prop_score_monoid_left_id
    it "identite droite" $ property prop_score_monoid_right_id
  describe "Logger Monad (instance perso)" $ do
    it "identite gauche  : pure x >>= f == f x"            $ property prop_logger_left_identity
    it "identite droite  : m >>= pure == m"                $ property prop_logger_right_identity
    it "associativite    : (m >>= f) >>= g == m >>= (\\x -> f x >>= g)" $ property prop_logger_associativity
    it "tell concatene les logs en ordre d'emission"        $ property prop_logger_tell_appends

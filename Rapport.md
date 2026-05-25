# Rapport projet PAF 2026 — Shoot'em up vertical

**Auteurs** : Bogdan Styn (`@21515753`) et Melissa Setbel (`@mSETBEL`).  
**Cursus** : Master STL — UE PAF 2026 — Sorbonne Université.

## 1. Description du jeu

On a fait un clone très simplifié de *Xenon 2: Megablast*. Le joueur pilote un vaisseau qui défile vers le haut, tire des projectiles, et doit survivre contre quatre types d'ennemis plus un boss qui se débloque à 500 points. Il y a aussi cinq bonus ramassables (santé, vitesse, invincibilité, plus deux qu'on a rajoutés nous-mêmes : inversion du scroll et tir triple), un système d'essais, un game over avec reset, et un couloir de murs latéraux qui zigzague et défile avec le fond. Au départ on parlait aussi de faire un mode multijoueur en réseau ; on a vite renoncé et finalement on a câblé un mode coop sur le même clavier en fin de projet (cf. §7).

Le jeu tourne avec Gloss à 60 FPS dans une fenêtre 566×358.

### Cartographie des questions du sujet (ER1)

Pour le correcteur : les questions du sujet sont traitées comme suit. Q1.1–1.6 via le type `Hitbox` (Point, Disque, Rectangle, Composee, MurGauche, MurDroit), les smart constructors `mkPoint`/`mkDisque`/`mkRectangle`, et la fonction `collision` étendue à toutes les paires utiles. Q2 via `worldWalls` qui construit un couloir `Composee [MurGauche, MurDroit]` (approximation finie à 5 jalons, cf. §3.4 du sujet et la justification §9). Q3 via les smart constructors validants côté Player/Ennemy/Bonus et la pile `State GameState` qui isole l'état mutable. Q4 via le `Player` et son `invincibleTimer` (i-frames après touche et au respawn). Q5 via le type somme `EnemyType` (Red/Yellow/Blue/Green/Boss) et un oracle pseudo-aléatoire fait main (LCG sur `playerX` dans `spawnYellowEnnemy`, cf. §7). Q6 via `gameLoop` (pile `State`) et `gameLoopM` (pile `ReaderT GameConfig (State GameState)`), cette dernière étant l'extension §3.3.

## 2. Manuel d'utilisation

### Prérequis

- `stack` (LTS récent compatible avec GHC 9.x),
- la bibliothèque `gloss` (déclarée dans `package.yaml`, installée automatiquement par stack),
- `hspec` et `QuickCheck` pour la suite de tests.

### Lancement

```bash
stack build
stack run
```

Si `stack run` produit un *segmentation fault* (problème connu avec certains drivers OpenGL sous Linux), lancer :

```bash
export LIBGL_ALWAYS_SOFTWARE=1
stack run
```

Pour les tests :

```bash
stack test
```

### Contrôles

| Touche | Action |
|--------|--------|
| ← →    | P1 — déplacement horizontal |
| ↑ ↓    | P1 — déplacement vertical |
| Espace | P1 — tirer |
| W A S D | P2 — déplacement (A/D = ←/→, W/S = ↑/↓) |
| F      | P2 — tirer |
| T      | Activer / désactiver le joueur 2 (mode coop sur le même clavier) |
| R      | Reset (redémarre la partie) |

### Ennemis

- **Red** : arrivent par groupes de 4 sur le côté, se déplacent horizontalement avec une oscillation verticale en sinus. 50 points.
- **Yellow** : entrent par le bas en diagonale et rebondissent sur les bords de l'écran. 75 points.
- **Blue** : se placent en haut et tirent des projectiles `Tear` vers le bas à intervalles réguliers. 100 points.
- **Green** : poursuivent le joueur (vecteur normalisé). Ils ont plusieurs HP : il faut plusieurs tirs pour les tuer. 150 points.
- **Boss** : apparaît dès que le score atteint 500, taille double (`bossCote = ennemyCote * 2`), 10 HP, descend lentement puis fait un va-et-vient horizontal en tirant. 1000 points.

### Bonus

- **Health** : +1 PV (plafonné à 5).
- **Speed** : augmente la vitesse pendant 500 frames.
- **Invincibility** : invincibilité pendant 500 frames.
- **ScrollReverse** (extension) : inverse le sens du défilement pendant 400 frames.
- **TripleShotBonus** (extension) : passe l'arme en mode triple pendant 600 frames (timer décrémenté à chaque tick).

## 3. Architecture du code

```
src/
  Model.hs      -- type GameState, logique de jeu, invariants, post/préconditions
  Vec2.hs       -- vecteur 2D, Functor/Applicative/Monoid
  Score.hs      -- newtype Score, Semigroup/Monoid
  Config.hs     -- GameConfig + pile ReaderT GameConfig (State GameState)
  Keyboard.hs   -- état du clavier (Set Key)
app/
  Main.hs       -- boucle Gloss (play), rendu, dispatch des événements
test/
  Spec.hs        -- point d'entrée hspec
  ModelSpec.hs   -- générateurs Arbitrary et propriétés du Model
  AlgebraSpec.hs -- lois Functor/Applicative/Monoid pour Vec2 et Score
```

Au départ, `Vec2` et `Score` vivaient dans `Model.hs`. Comme leurs lois sont testées dans `AlgebraSpec`, on les a extraits pour casser une dépendance circulaire qui commençait à pénétrer. `Config` est arrivée plus tard : tout était hardcodé en haut de `Model.hs` et on voulait pouvoir relancer le moteur avec d'autres valeurs (vitesse de scroll, seuil boss, etc.) sans rééditer du code partagé. On a sorti les constantes ajustables dans un record `GameConfig` et empilé un `ReaderT` au-dessus du `State`.

`Keyboard` enferme l'état du clavier dans un `Set Key` simplement pour ne pas mélanger la gestion des événements avec la logique du jeu. `Main` se contente de charger les assets, instancier `play`, et router les événements vers `execGameM` ou `execState`. Côté tests, on a coupé en deux (`ModelSpec` pour le gameplay, `AlgebraSpec` pour les lois) pour éviter une grosse classe. Le module `GameLogger` est venu à la toute fin : il porte une instance Monad personnalisée (cf. §6.4) sans dépendre du moteur.

## 4. Propriétés (par type et par opération)

Toutes les propriétés sont des fonctions Haskell `GameState -> Bool` (ou variante). Elles sont définies dans `src/Model.hs`. Le tableau qui suit suit la consigne du sujet : on classe d'abord par **type** (chaque sous-section liste les invariants spécifiques à un type du modèle), puis par **opération** (chaque sous-section liste les pré/postconditions associées à une transition).

### 4.1 Propriétés par type

#### Hitbox

- `prop_collision_composeePoints` (Q1.4) : si une `Composee` n'agglomère que des `Point`, une collision avec un autre `Point` impose l'égalité avec l'un des deux ; le but est de garantir que le cas générique `Composee` ne crée pas de faux positifs sur la primitive la plus restrictive.
- `prop_pre_mkPoint`, `prop_pre_mkDisque`, `prop_pre_mkRectangle` (Q1.2) : les smart constructors filtrent les hitbox dégénérées en amont ; sans ce filet, `collision` pourrait être appelée sur un `Disque` de rayon nul ou un `Rectangle` plat et renvoyer un résultat absurde.

#### Player

- `prop_inv_player` : nombre de PV, vies restantes, vitesse, position et timers d'arme/i-frames tous bornés. Sans cet invariant un overflow vers le négatif rendrait le respawn impossible (et l'écran finirait par afficher des coordonnées hors fenêtre).
- `prop_inv_lives` : `livesLeft ∈ [0, initLives]`. Cas critique : un bug dans `respawnPlayer` pourrait redonner indéfiniment des essais, ce qui briserait l'extension "essais multiples".

#### Player2 (coop)

- `prop_inv_player2` : si `player2 = Just p2`, alors `p2` satisfait les mêmes invariants que P1 et dispose d'un `livesLeft` **indépendant**. On rend explicite le fait que P2 n'est pas un miroir de P1.

#### Projectile

- `prop_inv_projectiles` : tout projectile a `sp > 0` et reste à l'intérieur de l'écran. Sans cela, un projectile fantôme à vitesse nulle resterait éternellement en bord de fenêtre et collisionnerait avec tout ce qui passe à proximité.

#### Ennemy (et Boss)

- `prop_inv_enemies` : ennemis `onScreen = True` cantonnés à la fenêtre, ennemis off-screen contraints seulement à `sp > 0`. La distinction reflète le fait que `spawn*Ennemy` commence systématiquement hors écran.
- `prop_inv_boss` : le boss stocke ses PV dans `ennemyPhase`. On exige `0 ≤ ennemyPhase ≤ bossHP` pour qu'un cumul de hits mal compté ne ressuscite jamais un boss déjà tué (cf. le bug "boss qui mourait avant d'apparaître", §9).
- `prop_inv_boss_unique` : au plus un boss à l'écran. Sans cette garde, le seuil de spawn à 500 points ferait apparaître un boss à chaque cycle et le score deviendrait infini.

#### Bonus

- `prop_inv_bonuses` : hitbox dans l'écran, et la durée (`Maybe Int`) est cohérente avec le type (Health sans durée, autres types avec la durée nominale). L'exhaustivité du `case` sur le constructeur de `BonusType` a fait remonter un bug réel (cf. §9 — `validBonus` non-exhaustive).

#### Mur (`worldWalls`)

- `prop_inv_worldWalls` : `worldWalls gs` renvoie toujours `Composee [MurGauche lps, MurDroit rps]` avec au moins 2 jalons par mur. C'est un invariant structurel : si la forme du couloir change accidentellement, les collisions `collision (worldWalls gs) cand` partent en vrille et le joueur traverse les parois.

#### GameState

- `prop_inv_GameState` : conjonction des invariants ci-dessus (Player, Projectiles, Enemies, Bonuses, Scroll, SpawnTimer, Score, ScrollReverse). Sert de précondition systématique aux propriétés via `==>`.
- `prop_inv_scroll` : `scrollOffset ∈ (-screenHeight, 0]`. Sans ce bornage, la rotation du fond se désynchroniserait sur quelques minutes.
- `prop_inv_score` : `score ≥ 0`. Le score n'est jamais décrémenté ; le contraire signalerait un mauvais usage du `Monoid Score`.
- `prop_inv_spawnTimer` : `ennemySpawnTimer ≥ 0 && bonusSpawnTimer ≥ 0`. Sans cela, le `case` de `updateEnnemies` qui matche sur les valeurs précises (1200, 900, 600, 0) finirait par ne plus jamais réveiller les spawns.
- `prop_inv_scrollReverse` : `scrollReverseTimer ≥ 0`. Le bonus d'inversion serait sinon "permanent" dès qu'on overflowerait vers le négatif.

### 4.2 Propriétés par opération

Chaque opération expose la conjonction `pré ⇒ post` qu'on vise. Les invariants implicitement préservés (cf. §4.1) ne sont pas répétés.

#### `moveLeft` / `moveRight` / `moveUp` / `moveDown`

- `prop_pre_moveLeft` / `prop_pre_moveRight` / `prop_pre_moveUp` / `prop_pre_moveDown` : il reste de la place dans la direction choisie (sinon le mouvement clamp). On expose la précondition pour qu'un test négatif distingue "ne s'est pas déplacé parce que pas possible" de "ne s'est pas déplacé à cause d'un bug".
- `prop_post_moveLeft` / `prop_post_moveRight` / `prop_post_moveUp` / `prop_post_moveDown` : soit la position devient le clamp attendu, soit elle reste inchangée (mouvement annulé par un mur). La disjonction est obligatoire depuis l'introduction du couloir.

#### `shoot` / `shootP2`

- `prop_pre_shoot` : `not (lost gs)` et hitbox rectangulaire ; on n'autorise pas le tir post game-over.
- `prop_pre_shootP2` : `not (lost gs)`, P2 présent et hitbox rectangulaire. **Dead-coded** côté tests (cf. §4.3) : la postcondition correspondante (`prop_post_shootP2_addsOneOrThree`) couvre déjà tous les cas et la précondition n'apporte pas d'information indépendante.
- `prop_post_shoot` : `+1` projectile en `SingleShot`, `+3` en `TripleShot`. Si `playerDead`, `+0` (no-op).
- `prop_post_shootP2_addsOneOrThree` : même règle pour P2, et `+0` si P2 absent.

#### `killEnnemy`

- `prop_pre_killEnnemy` : conjonction des trois invariants `player`, `enemies`, `projectiles`. **Dead-coded** côté tests : `killEnnemy` est appelé depuis `updateEnnemies` qui présuppose déjà l'invariant global, donc on ne wire pas cette précondition spécifique.
- `prop_post_killEnnemy_scoreMonotone` : le score ne décroît jamais.
- `prop_post_killEnnemy_enemyCount` : la liste d'ennemis ne croît jamais.
- `prop_post_killEnnemy_livesMonotone` : `livesLeft` ne peut qu'égaler ou décroître.
- `prop_post_killEnnemy_independentLives` : un joueur intact (PV > 0 et invincible ou pas touché) garde son `livesLeft`, même si l'autre consomme une vie ce frame. C'est la garantie formelle du pool indépendant.
- `prop_post_killEnnemy_gameOver` : `lost` reflète la conjonction `playerDead p1 && (player2 == Nothing || playerDead p2)`. Le but : empêcher un faux game over si P2 tient encore debout.

#### `togglePlayer2`

- `prop_post_togglePlayer2_involution` : deux toggles consécutifs préservent la présence/absence (le toggle est une vraie involution sur ce drapeau).
- `prop_post_togglePlayer2_alternates` : un seul toggle inverse strictement la présence (couvre l'autre sens).
- `prop_post_togglePlayer2_freshLives` : un P2 réactivé démarre avec `initLives` essais (chaque "session coop" a son propre stock).

#### `updateScroll`

- `prop_pre_updateScroll` : `scrollOffset > -screenHeight && scrollOffset ≤ 0`. **Dead-coded** côté tests : la précondition est strictement équivalente à `prop_inv_scroll` qui est déjà sous `==>`, on n'aurait que des tests redondants.
- `prop_post_updateScroll_reverse` : si `scrollReverseTimer > 0`, le nouvel offset est supérieur ou égal à l'ancien (le fond remonte).
- `prop_post_updateScroll_timer` : le timer d'inversion décroît et reste ≥ 0.

#### `updateProjectiles` / `updateBonuses` / `applyBonuses` / `updateWeaponTimer`

- `prop_post_updateProjectiles_noSpawn` : `updateProjectiles` ne crée jamais de projectile (filtrage + déplacement uniquement). Borne explicite : `|projectiles| + |enemies|` pour rester compatible avec `updateEnnemies` qui peut faire spawn un Tear.
- `prop_post_updateBonuses_atMostOneSpawn` : au plus un bonus ajouté par appel (le timer décide lequel, jamais deux).
- `prop_post_updateBonuses_preservesInv` : chaque bonus restant respecte encore `prop_inv_bonuses`.
- `prop_post_applyBonuses` : les bonus collidés disparaissent, la santé est monotone et plafonnée à 5, le `scrollReverseTimer` reste borné, et un `TripleShotBonus` touché par P1 passe son arme en `TripleShot`.
- `prop_post_applyBonuses_noGrowth` : la liste de bonus ne grandit jamais sous `applyBonuses` (consommation uniquement).
- `prop_post_updateWeaponTimer` : le timer d'arme décroît, et l'arme repasse en `SingleShot` dès qu'il atteint zéro.

#### `pushOutOfWalls`

- `prop_post_pushOutOfWalls` : après pushback, le joueur reste dans les bornes d'écran et, s'il collidait avec un mur, son `x` a bougé vers le centre. Garantit que la routine de secours ne projette pas le joueur hors fenêtre.

#### Smart constructors (Q1.2)

- `prop_pre_mkPoint` : accepte tout couple (le `Point` n'a pas d'invariant non trivial).
- `prop_pre_mkDisque` : retourne `Just` ssi rayon > 0.
- `prop_pre_mkRectangle` : retourne `Just` ssi largeur > 0 et hauteur > 0.

#### Boucle de jeu (`gameLoopM`, Q6)

- `property_gameLoopM_preservesInv` : exécuter une frame complète depuis n'importe quel `GameState` valide et n'importe quel vecteur de touches (4 booléens) préserve l'invariant global. Garantit que la pile `ReaderT GameConfig (State GameState)` n'introduit pas de régression par rapport aux versions pures.

#### `resetGame`

- `prop_post_resetGame` : après `runState resetGame initGameState`, on retrouve un état canonique (score à 0, listes vides, arme `SingleShot`, `livesLeft = initLives`). Test "concret" (non quantifié) qui sert d'oracle minimal.


## 5. Tests

Les tests utilisent `hspec` comme runner et `QuickCheck` pour le property-based testing. Le point d'entrée est `test/Spec.hs`. Les spécifications sont réparties dans `test/ModelSpec.hs` et `test/AlgebraSpec.hs`.

### 5.1 Organisation par `describe`

`test/Spec.hs` chaîne dix blocs `describe`.

- **`initGameStateSpec`** : un seul test, vérifie que l'état initial respecte `prop_inv_GameState`. Sert d'oracle de départ : si cet invariant casse, tous les autres tests perdent leur sens.
- **`moveLeftSpec`, `moveRightSpec`, `moveUpSpec`, `moveDownSpec`** : pour chaque direction, on teste la conservation d'invariant ET la postcondition. Quatre blocs distincts plutôt qu'un seul "movesSpec" parce qu'on veut une trace claire quand un seul axe casse.
- **`shootSpec`** : compte exact de projectiles ajoutés selon `weaponMode`. Couvre directement l'extension tir triple.
- **`scrollSpec`** : invariant + monotonie du défilement (qui peut être inversée par le bonus `ScrollReverse`, d'où la disjonction `sc' < sc || sc' == 0`).
- **`projectileSpec`** : `updateProjectiles` ne déplace personne hors fenêtre et ne produit jamais de compte négatif.
- **`ennemySpec`** : monotonie de `onScreen` (une fois à l'écran, on y reste), conservation d'invariant après mouvement, timer de spawn ≥ 0. Le premier test attrape les bugs où un ennemi disparaîtrait du flag puis serait spawnable une deuxième fois.
- **`bonusSpec`** : `applyBonuses` respecte sa postcondition même sur `genGameStateRich` (états avec plusieurs bonus du même type), `updateBonuses` préserve l'invariant des bonus.
- **`scoreSpec`** : `score ≥ 0`, et `killEnnemy` est monotone à la fois sur le score, sur le nombre d'ennemis et sur les vies.
- **`extensionsSpec`** : le gros bloc fourre-tout pour tout ce qui touche aux extensions (scroll inversé, mur, timer d'arme, pushback, essais multiples, resetGame, coop avec ses six propriétés P2/togglePlayer2, et les deux propriétés Boss `prop_inv_boss` / `prop_inv_boss_unique`). On a regroupé ici parce que ces tests partagent le même générateur `Arbitrary GameState` mélangé. Un découpage plus fin aurait dupliqué de la cosmétique sans gagner en lisibilité.
- **`genSpec`** : auto-tests sur les générateurs eux-mêmes (`genGameStateOk` produit valide, `genGameStateFree` peut produire invalide via `expectFailure`, etc.). Ce bloc est notre filet contre les générateurs trompeurs (cf. §9 — bug `validBonus` non-exhaustive caché par un générateur vide).
- **`hitboxSpec`** : les trois smart constructors Q1.2 (`mkPoint`, `mkDisque`, `mkRectangle`) et la propriété Q1.4 `prop_collision_composeePoints`.
- **`gameMSpec`** : test d'intégration de la pile `ReaderT GameConfig (State GameState)`. Vérifie qu'une frame complète depuis n'importe quel état valide préserve les invariants ; c'est le test qui légitime concrètement l'extension §3.3 sans se contenter de tester chaque sous-fonction isolément.
- **`algebraSpec`** : lois universelles de `Functor Vec2`, `Applicative Vec2`, `Monoid Vec2`, `Monoid Score`, et les trois lois `Monad` (gauche, droite, associativité) de l'instance personnalisée `Logger` (cf. §6.4) plus une postcondition sur `tell` qui garantit l'ordre d'émission.

### 5.2 Méthodologie des générateurs

On a écrit trois générateurs de `GameState`, mélangés via `frequency` dans l'instance `Arbitrary`. Les poids `(2, 3, 5)` ne sont pas arbitraires : le générateur "riche" pèse plus parce que c'est lui qui exerce vraiment les transitions (ennemis présents, P2 présent une fois sur deux, projectiles non vides). Le générateur libre pèse 2 (≈20 %), juste assez pour que la précondition `==>` retourne un nombre raisonnable de cas vraiment exercés sans noyer les tests dans des cas rejetés. Le générateur `Ok` pèse 3, pour avoir une masse stable de cas "lisses" qui consomment peu de temps.

```haskell
instance Arbitrary GameState where
  arbitrary = frequency [(2, genGameStateFree), (3, genGameStateOk), (5, genGameStateRich)]
```

Les trois générateurs et leur intention :

- **`genGameStateFree`** : tire des coordonnées et timers sans contrainte forte, donc produit parfois des états invalides. Sa raison d'être : `expectFailure` dans `genSpec` détecte un faux générateur (s'il devenait trop "doux" et ne produisait plus jamais d'invalide, le test échouerait).
- **`genGameStateOk`** : garantit l'invariant. Cible des postconditions qui supposent l'invariant en entrée. Listes vides (`enemies = []`, etc.) pour rester rapide.
- **`genGameStateRich`** : démarre de `genGameStateOk` puis attache des listes générées (`genEnnemyOnScreen`/`genEnnemyOffScreen` pondérés `(7, 3)`, `genProjectile`, `genBonus`), un boss optionnel (`Maybe`, fréquence `(3, 1)`), et un P2 optionnel (`Maybe`, fréquence `(1, 1)`). C'est ce qui révèle les bugs croisés (le bug de `applyBonuses` qui ne comptait pas P2 a été attrapé là).

Le combinateur `==>` (`prop_inv_GameState gs ==> ...`) sert de précondition à la Hoare : les états qui ne respectent pas l'invariant sont simplement écartés du compte. On accepte ~20 % de rejets comme tarif normal.

### 5.3 Test négatif sur le générateur libre

Pour démontrer que `genGameStateFree` est vraiment capable de produire des états invalides, on utilise `expectFailure` :

```haskell
describe "genGameStateFree" $
  it "can generate invalid states (expected)" $ expectFailure $
    property property_inv_genGameStateFree
```

Si le générateur libre redevenait silencieusement correct, le test échouerait. Garantie sur la qualité du générateur, pas sur la fonction testée.

### 5.4 Bilan

68 examples, 0 failures (58 cas dans `ModelSpec`/`hitboxSpec`/`gameMSpec`, 10 cas dans `AlgebraSpec`). Sortie typique :

```
Finished in 0.10 seconds
68 examples, 0 failures
```

## 6. Structures algébriques

### `Vec2 a` (Functor + Applicative + Semigroup + Monoid)

Définie dans `src/Vec2.hs`. Les instances Functor et Applicative permettent d'écrire l'arithmétique vectorielle sans dupliquer le code composante par composante :

```haskell
instance Functor Vec2 where
  fmap f (Vec2 x y) = Vec2 (f x) (f y)

instance Applicative Vec2 where
  pure x = Vec2 x x
  (Vec2 f g) <*> (Vec2 x y) = Vec2 (f x) (g y)

vadd u v = (+) <$> u <*> v
vscale k = fmap (* k)
```

Le Monoid (addition vectorielle, neutre `Vec2 0 0`) sert à cumuler des déplacements avec `foldMap`.

En pratique, on s'en sert dans `moveGreenEnnemy` pour calculer la direction de poursuite du joueur :

```haskell
let pos        = Vec2 cx cy
    target     = Vec2 playerCx playerCy
    dir        = vnormalize (vsub target pos)
    Vec2 nx ny = vadd pos (vscale sp dir)
```

Avant `Vec2`, `moveGreenEnnemy` faisait à la main `(nx, ny) = (cx + sp * dx, cy + sp * dy)` avec un `vnorm` recalculé sur place. C'est ce qui nous a décidés à factoriser : on perdait du temps à chaque ajout de calcul vectoriel à se retaper la même arithmétique.

### `Score` (Semigroup + Monoid)

Définie dans `src/Score.hs` comme un `newtype Score = Score { unScore :: Int }`. L'instance Monoid additionne les scores avec neutre `Score 0`.

Le newtype existe pour éviter d'utiliser directement le Monoid de `Sum Int` et pour avoir un type spécifique au score. L'addition est associative et commutative, donc l'ordre dans lequel on tue les ennemis n'a aucune importance, propriété exploitée dans `killEnnemy` :

```haskell
let gained = scoreValue $ foldMap (Score . pointsForType . ennemyType) dead
```

Le `foldMap` parcourt la liste des ennemis tués au cours du tick, calcule les points de chacun, et les cumule via `<>`. On n'a pas eu besoin de pli explicite et l'ordre de la liste `dead` n'a pas d'effet — propriété qu'on n'a pas testée formellement parce qu'elle découle de la commutativité de `(+)` sur `Int`.

### Pile `ReaderT GameConfig (State GameState)`

Définie dans `src/Config.hs` :

```haskell
type GameM a = ReaderT GameConfig (State GameState) a
```

`GameConfig` regroupe les paramètres ajustables (vitesse de scroll, taille d'écran, vitesse de base et boostée du joueur, fréquence de spawn, durée d'invincibilité après touche). Trois fonctions de la boucle de jeu sont câblées en `GameM` et lisent réellement la config au lieu d'utiliser les constantes hardcodées du Model :

```haskell
updateSpeedTimerCfg :: GameM ()
updateSpeedTimerCfg = do
  base  <- asks cfgPlayerSpeed
  boost <- asks cfgPlayerSpeedBoost
  modify $ \gs ->
    let pl = player gs
        newPl = if speedyTimer pl > 0
                then pl { persoSpeed = boost, speedyTimer = speedyTimer pl - 1 }
                else pl { persoSpeed = base }
    in gs { player = newPl }
```

`updateScrollCfg` lit `cfgScrollSpeed` et `cfgScreenHeight`, et `killEnnemyCfg` lit `cfgInvincibilityFr` qu'elle passe à la version paramétrée `killEnnemyWith`. La boucle principale enchaîne :

```haskell
gameLoopM [left, right, up, down] = do
  lift $ do ... actions purement State ...
  updateScrollCfg
  lift updateBonusesM
  updateSpeedTimerCfg
  lift updateWeaponTimerM
```

Câblage dans `app/Main.hs` :

```haskell
GameControl kbd (execGameM defaultConfig gs (gameLoopM keys))
```

Concrètement, on peut désormais relancer le jeu avec une `GameConfig` modifiée (par exemple `defaultConfig { cfgScrollSpeed = 4 }`) sans toucher au Model. Le test d'intégration `gameMSpec` (cf. §5.1) prend un `GameState` valide et un vecteur de 4 touches arbitraire, lance `execGameM defaultConfig gs (gameLoopM keys)`, et vérifie que tous les invariants tiennent encore. C'est ce qui valide concrètement la pile `ReaderT GameConfig (State GameState)` au-delà des tests unitaires sur les sous-fonctions.

### 6.4 Une instance Monad personnalisée — `Logger`

Le sujet (§3.3) demande une instance Monad maison. On a choisi de créer le `Logger`.

`src/GameLogger.hs` définit :

```haskell
newtype Logger w a = Logger { runLogger :: (a, [w]) }

instance Monad (Logger w) where
  return = pure
  Logger (a, ws1) >>= k =
    let Logger (b, ws2) = k a
    in Logger (b, ws1 ++ ws2)

tell :: w -> Logger w ()
tell w = Logger ((), [w])
```

L'instance Functor et Applicative sont également écrites à la main pour rester homogène (on évite le `derive`). On accumule les logs avec `(++)` côté droit, ce qui rend les trois lois Monad immédiates : l'identité gauche revient à `[] ++ ws == ws`, l'identité droite à `ws ++ [] == ws`, et l'associativité à `(ws1 ++ ws2) ++ ws3 == ws1 ++ (ws2 ++ ws3)` — toutes vraies par construction de `[]`.

Les trois lois sont testées via QuickCheck dans `test/AlgebraSpec.hs` (bloc `Logger Monad (instance perso)`) :

```haskell
prop_logger_left_identity  x (Fn f) = runLogger (pure x >>= f) == runLogger (f x)
prop_logger_right_identity m        = runLogger (m >>= pure)   == runLogger m
prop_logger_associativity  m (Fn f) (Fn g) =
  runLogger ((m >>= f) >>= g) == runLogger (m >>= (\x -> f x >>= g))
```

On y a ajouté `prop_logger_tell_appends` pour vérifier que l'ordre d'émission est préservé (un Monad qui aurait remplacé `(++)` par un `flip (++)` passerait les trois lois, mais pas celle-là).

### 6.5 Smart constructors et choix `error` vs `Either`

Trois smart constructors retournent `Maybe` (`mkPoint`, `mkDisque`, `mkRectangle`) pour matérialiser la précondition Q1.2 sans plomber les call sites. Trois autres (`mkPlayer`, `mkEnnemyHP`, `mkBonus`) utilisent `error` sur entrée invalide.

## 7. Extensions implémentées

On a fini par implémenter 7 extensions au total.

### Système de score

Barème :

| Type   | Points |
|--------|--------|
| Red    | 50     |
| Yellow | 75     |
| Blue   | 100    |
| Green  | 150    |
| Boss   | 1000   |

Le score est stocké dans `GameState.score :: Int` et affiché en haut à gauche via `Text` de Gloss :

```haskell
scorePic = Translate (-screenWidth/2 + 10) (screenHeight/2 - 25)
         $ Scale 0.15 0.15
         $ Color white
         $ Text ("SCORE " ++ show (score gs))
```

Le cumul utilise le Monoid `Score` (cf. section 6).

### Essais multiples

On a ajouté `livesLeft :: Int` au `Player`. La valeur initiale est `initLives = 2` (soit 3 vies au total avec la barre courante). Quand `persoHealth` atteint 0 et qu'il reste au moins un essai :

```haskell
respawning = persoHealth newPlayer <= 0 && livesLeft player > 0
respawnedPlayer
  | respawning =
      newPlayer { persoHealth = respawnHealth        -- 5
                , livesLeft = livesLeft player - 1
                , invincibleTimer = respawnInvincibility  -- 200
                , weaponMode = SingleShot
                , persoHitbox = persoHitbox initPlayer }
```

Au respawn, on remet 5 PV, on décrémente le compteur d'essais, on accorde 200 frames d'invincibilité, on remet l'arme en mode simple et on replace le joueur à sa position initiale. On vide aussi `enemies` et `projectiles` le temps du respawn pour éviter une mort instantanée.

Le game over réel survient seulement quand `livesLeft == 0 && persoHealth == 0`.

Affichage : un mini-vaisseau pour chaque essai restant, à gauche de la barre de vie.

### Bonus d'inversion du scroll

- Nouveau constructeur `ScrollReverse` dans `BonusType`.
- Nouveau champ `scrollReverseTimer :: Int` dans `GameState`.
- Durée : `scrollReverseBonusDuration = 400` frames.
- `updateScroll` inverse le signe du pas tant que le timer est strictement positif :

```haskell
updateScroll gs =
  let dir  = if scrollReverseTimer gs > 0 then 1 else -1
      step = fromIntegral dir * scrollSpeed
      ...
```

Le timer décroît à chaque frame (postcondition `prop_post_updateScroll_timer`) et le défilement repart vers le bas une fois épuisé.

### Tir étendu (triple shot)

- Type `WeaponMode = SingleShot | TripleShot` et champs `weaponMode`, `weaponTimer` dans `Player`.
- Bonus `TripleShotBonus` qui passe l'arme en mode triple et arme le timer à `tripleShotBonusDuration = 600` frames.
- `shoot` produit 1 ou 3 projectiles selon le mode :

```haskell
TripleShot -> [ initProjectile muzzleX        muzzleY UpDir Bullet
              , initProjectile (muzzleX - 10) muzzleY UpDir Bullet
              , initProjectile (muzzleX + 10) muzzleY UpDir Bullet
              ]
```

`updateWeaponTimer` décrémente `weaponTimer` à chaque tick et repasse en `SingleShot` quand il atteint zéro. L'invariant `prop_inv_player` exige `weaponTimer > 0 ⟺ weaponMode == TripleShot`, et la postcondition `prop_post_updateWeaponTimer` vérifie que le timer décroît bien. La postcondition `prop_post_shoot` distingue les deux cas (`+1` vs `+3`).

### Boss

- Nouveau constructeur `Boss` dans `EnemyType`.
- Taille double : `bossCote = ennemyCote * 2`.
- 10 HP stockés dans `ennemyPhase` (réutilisation du champ qui sert déjà pour les compteurs d'oscillation des autres ennemis).
- Spawn déclenché par `score >= bossScoreThreshold` (500), uniquement s'il n'y a pas déjà un boss à l'écran.
- Comportement : descente lente jusqu'à `cy = 80`, puis va-et-vient horizontal, tir périodique de `Tear`. Le compteur de recharge (`bossReloadFrames = 60`) est stocké dans la composante `dy` de `ennemyDirection` pour éviter qu'il ne se mélange avec les PV (cf. section 9, bug rencontré pendant les tests).
- Mort : 10 hits de `Bullet`. Donne 1000 points.
- Sûreté : deux propriétés dédiées dans `extensionsSpec`. `prop_inv_boss` garantit `0 ≤ ennemyPhase ≤ bossHP` (un boss ne peut pas dépasser ses 10 PV maximum ni descendre en négatif), et `prop_inv_boss_unique` interdit la duplication à l'écran. Le générateur riche `genGameStateRich` produit un boss optionnel (`Maybe Ennemy` avec poids `(3, 1)`) pour que ces deux propriétés soient effectivement exercées et non triviales.

### Murs latéraux (Composee + MurGauche + MurDroit)

`worldWalls` construit un couloir zigzaguant qui dépend du `scrollOffset` (donc qui défile avec le fond) :

```haskell
worldWalls gs =
  let off       = scrollOffset gs
      wobble y  = 25 * sin ((y + off) * 0.015)
      ys        = [-screenHeight, -screenHeight/2, 0, screenHeight/2, screenHeight]
      leftPts   = [ (leftEdge  + 25 + wobble y, y) | y <- ys ]
      rightPts  = [ (rightEdge - 25 - wobble y, y) | y <- ys ]
  in Composee [MurGauche leftPts, MurDroit rightPts]
```

Les fonctions `moveLeft/Right/Up/Down` testent la collision du candidat (Rectangle) contre `worldWalls gs` et annulent le déplacement le cas échéant : le joueur est *repoussé* par la paroi plutôt que d'être autorisé à la traverser. La collision `MurGauche/MurDroit` vs `Rectangle` teste les deux coins concernés du joueur (cf. section 9 pour la justification de cette extension).

L'oracle de spawn de l'ennemi jaune est lui aussi codé à la main. On utilise un générateur linéaire sur la position courante du joueur :

```haskell
seed   = round playerX :: Int
spawnX = fromIntegral ((seed * 1103515245 + 12345) `mod` round screenWidth) - screenWidth / 2
```

### Mode coopératif à deux joueurs (même clavier)

```haskell
data GameState = GameState { ..., player2 :: Maybe Player }
```

`Maybe` plutôt que `[Player]` parce que le nombre de joueurs est borné à deux, et surtout pour éviter de réécrire les invariants existants sur `player` quand P2 est absent — `prop_inv_player` reste tel quel. On a aussi ajouté un toggle `T` qui active/désactive P2 à la volée.

Côté contrôles :

| Joueur 1 | Joueur 2 |
|---|---|
| ←/→/↑/↓ | W/A/S/D |
| Tir : Espace | Tir : F |
| Toggle P2 : T |

Sur les choix de design, le point sur lequel on a hésité le plus longtemps est le score. Compteur global ou un par joueur ? On a tranché pour global parce que c'est cohérent avec un coop d'arcade.

Pour les vies (`livesLeft`), on a choisi un compteur **indépendant** par joueur : un joueur fragile ne pénalise pas son coéquipier, et chacun gère son propre stock d'essais. Les PV par vie (`persoHealth`) sont eux aussi indépendants : chacun encaisse ses hits jusqu'à 0 PV avant de consommer une vie de son propre pool. Le game over n'est déclenché que quand **P1 est totalement KO (PV = 0 et essais = 0) ET P2 absent ou lui aussi totalement KO**.

Quand P2 atteint cet état définitif, on le retire du jeu (`player2 = Nothing`) ; P1 reste à l'écran mais ses contrôles deviennent des no-ops (cf. les gardes `playerDead` sur `moveUp/Down/Left/Right/shoot`).

Le refactor le plus invasif a été celui de `killEnnemyWith`. On l'a découpé en deux helpers (`stepPlayer` pour le tick HP/i-frames d'un joueur, `respawnPlayer` pour la logique de respawn) appliqués indépendamment à P1 et à `player2 gs`, de sorte que chaque joueur consomme son propre `livesLeft`. `applyBonuses` itère via un helper `applyToPlayer` : si les deux joueurs touchent le même bonus dans la même frame, les deux en bénéficient. Seul `ScrollReverse` reste global, parce qu'il n'y a qu'un seul scroll.

Du côté des updates, les anciens `updateSpeedTimer` et `updateWeaponTimer` opéraient sur `player gs` uniquement. On a extrait un step (`stepW`, `stepSp`) qu'on applique à la fois à P1 et à `fmap stepW (player2 gs)`.

Trois propriétés ont accompagné la première version du coop : `prop_inv_player2` (P2 satisfait les mêmes invariants que P1, **pool de vies indépendant** désormais), `prop_post_togglePlayer2_involution` (deux toggles consécutifs préservent la présence/absence), et `prop_post_togglePlayer2_alternates` (un seul toggle inverse strictement la présence).

On les a complétées par quatre propriétés supplémentaires. `prop_post_togglePlayer2_freshLives` vérifie qu'un nouveau P2 démarre avec `initLives` essais. `prop_post_killEnnemy_independentLives` certifie que les pools ne s'influencent pas (un joueur intact ne perd jamais d'essai à cause de l'autre). `prop_post_killEnnemy_gameOver` reflète la conjonction `playerDead p1 && (p2 == Nothing || playerDead p2)`. Enfin `prop_post_shootP2_addsOneOrThree` impose que P2 tire selon son `weaponMode` exactement comme P1.

Le générateur riche tire `player2` à pile/face entre `Nothing` et `Just genPlayerOk`. Ça exerce automatiquement tous les chemins coop dans les ~10 propriétés qui consomment `genGameStateRich` (notamment `applyBonuses`, `killEnnemy`), sans avoir besoin d'ajouter une dizaine de tests dédiés. Et ça a immédiatement fait remonter un bug : `prop_post_applyBonuses` ne comptait que les bonus collidés par P1, alors qu'en coop un bonus peut disparaître parce que P2 l'a ramassé. On a corrigé la postcondition en prenant la disjonction `collisionP1 || collisionP2`.

## 8. Code qu'on trouve intéressant à montrer

### Cumul du score via `foldMap`

```haskell
let gained = scoreValue $ foldMap (Score . pointsForType . ennemyType) dead
```

Au départ, on avait écrit ça avec `sum (map pointsForType …)`. On a basculé sur `foldMap` pour avoir un cas d'usage non-trivial du Monoid `Score` qu'on avait défini juste pour ça, sans ça le newtype `Score` ne servait qu'à hériter de `Sum Int`.

### `worldWalls` qui exploite la composition

La collision contre un couloir entier est gérée par le cas générique `collision (Composee hs) other = any (`collision` other) hs`, sans code spécifique au "couloir".

## 9. Difficultés rencontrées

### Patterns positionnels vs syntaxe record

À chaque ajout d'un champ dans `GameState` ou `Player`, tous les patterns positionnels (`GameState _ _ _ _ _ _ _`) ont cassé. On a vécu avec pendant la majeure partie du projet, puis on a fini par tout convertir à la syntaxe record / aux accessors nommés sur les sites critiques. Maintenant l'ajout d'un champ ne casse plus que les fonctions qui le lisent vraiment.

### Pile ReaderT / State

On a décidé de garder `type Game a = State GameState a` et utiliser `lift` pour exposer la boucle existante depuis `GameM`. Une fois ce câblage en place, on a converti progressivement trois fonctions clés en `GameM` (`updateScrollCfg`, `updateSpeedTimerCfg`, `killEnnemyCfg`) qui lisent réellement la `GameConfig` via `asks`.

### Tir triple

Le sujet ne tranche pas. Une première version "permanente jusqu'au respawn" était plus simple mais incohérente avec les autres bonus temporels. On a finalement opté pour un `weaponTimer` similaire à `speedyTimer`, géré par `updateWeaponTimer`, qui repasse en `SingleShot` quand il atteint zéro. Le bonus dure `tripleShotBonusDuration = 600` frames. Cela donne une vraie postcondition testable : `weaponMode == TripleShot ⟺ weaponTimer > 0`.

### Collisions murs vs joueur

Le sujet définit `MurGauche` / `MurDroit` avec une collision contre un `Point`. On a dû étendre `collision` à `(MurGauche, Rectangle)` (et symétrique `MurDroit`) pour pouvoir repousser le joueur qui est un Rectangle :

```haskell
collision (MurGauche segs) (Rectangle x y _ h) =
  collision (MurGauche segs) (Point x y) || collision (MurGauche segs) (Point x (y+h))
```

On teste les deux coins du côté gauche du joueur contre le mur gauche, et symétriquement à droite.

### Postconditions des mouvements après ajout des murs

Les anciennes postconditions exigeaient l'égalité stricte `y2 == min (...) (y + sp)`. Une fois les murs ajoutés, `moveUp` peut annuler le déplacement, ce qui faisait planter les tests. On a introduit la disjonction `y2 == ... || y2 == y` (joueur repoussé par un mur) pour refléter ce nouveau comportement sans casser l'esprit des propriétés existantes.

### Bugs attrapés par les tests et la revue

C'est la partie qu'on a trouvée la plus formatrice. Quatre exemples concrets :

1. **Boss qui mourait avant d'apparaître à l'écran.** Première implémentation : `ennemyPhase` du boss servait à la fois de PV (décrément à chaque hit) et de compteur de recharge (décrément à chaque tick, remis à 60 quand il tire). Le boss perdait toutes ses HP en 10 frames *avant même d'entrer à l'écran*, ce qui donnait 1000 points "gratuits" au joueur dès qu'il atteignait le seuil de spawn, en boucle toutes les 25 secondes. On a séparé les deux compteurs : `ennemyPhase` ne sert plus qu'aux PV, et le compteur de recharge va dans la composante `dy` de `ennemyDirection` (champ inutilisé pour un boss qui ne se déplace qu'horizontalement). Bug repéré par la revue, pas par les tests : on en a tiré une leçon — il faut tester aussi les *transitions sur plusieurs frames*, pas seulement les opérations isolées.

2. **`validBonus` non-exhaustive.** L'ajout des bonus `ScrollReverse` et `TripleShotBonus` a laissé la postcondition `prop_post_applyBonuses` avec un `case` non-exhaustif. Le test passait quand même, parce que `genGameStateOk` produisait toujours `bonuses = []`, donc `all f []` était trivialement vrai. On a ajouté un générateur riche (`genGameStateRich`) au mélange de l'instance `Arbitrary`, ce qui a immédiatement fait planter le test, et on a complété les cas manquants. Le générateur "vide" cachait la faille.

3. **Postconditions de mouvement cassées par les murs.** L'introduction des murs a fait apparaître des contre-exemples QuickCheck sur `prop_post_moveLeft` : le joueur ne se déplaçait pas quand un mur le repoussait, mais la postcondition exigeait l'égalité avec la position clampée. On a relâché en `clamped || x' == x`. C'est l'exemple type d'une postcondition trop forte qui devient fausse quand on enrichit le code.

4. **Joueur immobile englobé par le mur défilant.** Le mur a une mouvement sinusoïdale qui défile avec le scroll : si le joueur reste immobile, une crête du mur peut "absorber" sa hitbox. Dans cet état, toute tentative de déplacement échoue parce que chaque candidat est aussi en collision. On a ajouté `pushOutOfWalls` appelé à la fin de `updateScroll` : tant que la hitbox du joueur intersecte les murs, on le décale vers le centre par paliers de 4 px (max 10 itérations). La postcondition `prop_post_pushOutOfWalls` vérifie que la nouvelle position reste dans les bornes d'écran et que le déplacement, s'il y en a un, va vers le centre.

### Smart constructors

Pour éviter de propager des `Player` ou `Ennemy` invalides, on a introduit trois constructeurs validants : `mkPlayer` (vitesse > 0, HP ≥ 0, lives ≥ 0, hitbox rectangulaire de taille > 0), `mkProjectile` (impose la `Disque` qu'attend `collision`) et `mkEnnemyHP` (HP > 0, sinon l'ennemi serait `killed` dès la naissance).

Côté `Hitbox`, on a complété par trois constructeurs Q1.2 retournant `Maybe` : `mkPoint` accepte tout couple, `mkDisque` impose un rayon strictement positif, `mkRectangle` impose largeur et hauteur strictement positives. Les invariants associés sont couverts par `prop_pre_mkPoint`/`mkDisque`/`mkRectangle`.

On a aussi ajouté la propriété Q1.4 `prop_collision_composeePoints` qui exprime : si `Composee [Point a b, Point c d]` collisionne avec `Point h2`, alors `h2 ∈ {Point a b, Point c d}` (la collision Point/Point est l'égalité stricte). Les `init*` sont réécrites comme appels à ces smart constructors : on garde les data constructors visibles pour le pattern matching, mais tout nouveau code passe par les `mk*`. Le choix de `error` vs `Either` est argumenté en §6.5.

## 10. Bilan et axes d'amélioration

### Ce qui n'a pas été fait

- **High-scores persistants** : aurait demandé d'écrire dans un fichier. On y a pensé tard et on a préféré durcir les propriétés existantes.
- **Animation des explosions** : aujourd'hui les ennemis disparaissent d'un coup quand `killEnnemy` les filtre — un sprite d'explosion serait sympa visuellement.
- **Sons / musique** : Comme Gloss n'a pas d'audio natif, il aurait fallu binder SDL ou ALUT. Pas prioritaire vu le reste.

### Améliorations possibles

- **Système de niveaux** : on pourrait piloter les apparitions d'ennemis par une liste paresseuse `[(Frame, SpawnEvent)]`, ce qui permettrait de scripter des vagues sans toucher au cœur du moteur.
- **Polir le mode coop** : écran d'attente entre respawns plutôt qu'un simple flash, menu d'activation explicite à l'écran-titre, écran de répartition des touches modifiable.
- **Utiliser `GameLogger`** : instrumenter `killEnnemy` pour produire une trace `[KillEvent]` consultable depuis l'écran de game over.

# PAF ShootEmUp — Xenon-like

Projet PAF 2026 (Master STL, Sorbonne) — un *shoot 'em up* vertical inspiré de Xenon 2: Megablast, écrit en Haskell + Gloss.

## Lancement

```sh
stack run
```

Si `stack run` donne une *segmentation fault*, lancer avant :

```sh
export LIBGL_ALWAYS_SOFTWARE=1
```

## Commandes

| Action | Joueur 1 | Joueur 2 (coop) |
|---|---|---|
| Bouger | flèches ← ↑ → ↓ | W (haut), A (gauche), S (bas), D (droite) |
| Tirer | espace | F |
| Activer / désactiver coop | T | T |
| Reset | R | R |

Chaque joueur dispose de son propre pool d'essais. Le *game over* n'arrive que lorsque P1 **et** P2 sont à zéro PV **et** zéro essai (si P2 est absent, seul P1 compte). P2 disparaît de l'écran à sa mort définitive ; P1 reste affiché mais figé.

Bonus ramassables : santé (cœur), vitesse, invincibilité, armement triple, inversion de défilement.

## Tests

```sh
stack test
```

61 specs HSpec + propriétés QuickCheck (invariants, pré/postconditions sur tous les opérateurs gameplay).

## Rapport

Le rapport détaillé (en français) est dans [`Rapport.md`](Rapport.md).

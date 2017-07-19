<!-- python -m readme2tex --usepackage "tikz" --usepackage "xcolor" --output README.md --readme  READOTHER.md --nocdn --pngtrick -->

# PPattern - Permutation Pattern

## Permutations

### Implementation

Permutations are implemented as lists of points with increasing x-coordinates.

```haskell
-- Define in Data.Algorithm.PPattern.Geometry.Point.hs
newtype Point = Point (Int, Int) deriving (Show, Eq, Ord)

-- Define in Data.Algorithm.PPattern.Perm.hs
newtype Perm = Perm { getList :: [Point] } deriving (Eq, Ord)
```

The function `mk :: (Foldable t, Ord a) => t a -> Perm` is devoted to creating
permutations from foldable objects.

```haskell
λ: Perm.mk [2,1,3]
[2,1,3]
λ: Perm.mk "bac"
[2,1,3]
λ: Perm.mk ["tomorrow", "today", "yesterday"]
[2,1,3]
```

### Basic manipulation

```haskell
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: let p = Perm.mk "acedb"
λ: p
[1,3,5,4,2]
λ: Perm.size p
5
λ: Perm.points p
[Point (1,1),Point (2,3),Point (3,5),Point (4,4),Point (5,2)]
λ: Perm.xCoords p
[1,2,3,4,5]
λ: Perm.yCoords p
[1,3,5,4,2]
```

`Perm.xCoords` and `Perm.yCoords` are reduced forms for:

```haskell
λ: import qualified Data.Algorithm.PPattern.Geometry.Point as Point
λ: fmap Point.xCoord (Perm.points p)
[1,2,3,4,5]
λ: fmap Point.yCoord (Perm.points p)
[1,3,5,4,2]
```

As you might have guessed, `show` for permutations reduces to `show . yCoords`:

```haskell
instance Show Perm where
  show = show . yCoords
```

### Basic properties

### Ties

Ties are allowed and are resolved according to the left-to-right order.

```haskell
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: Perm.mk "acb"
[1,3,2]
λ: Perm.mk "acbacb"
[1,5,3,2,6,4]
λ: Perm.mk "acbacbacb"
[1,7,4,2,8,5,3,9,6]
```

### Basic operations

The *reverse* of a permutation $\sigma = \sigma_1 \sigma_1 \ldots \sigma_n$
is the permutation $\sigma_n \sigma_{n-1} \ldots \sigma_1$.
The *complement* of $\sigma$ is the permutation
$\beta_1 \beta_2 \ldots \beta_n$, where
$\beta_i = n+1-\sigma_i$.
That is, the complement substitutes the largest element of a permutation
by the smallest one, the next largest element by the next smallest element, etc.
The *inverse* of $\sigma$ is the permutation where the $\sigma_i$-th position
is occupied by $i$.

```haskell
λ: import qualified Data.Algorithm.PPattern.APerm as APerm
λ: import qualified Data.Algorithm.PPattern.Perm.Operation as Perm.Operation
λ: let p = Perm.mk [1,4,2,5,3]
λ: Perm.Operation.reversal p
[3,5,2,4,1]
λ: Perm.Operation.complement p
[5,2,4,1,3]
λ: Perm.Operation.reversalComplement p
[3,1,4,2,5]
λ: (Perm.Operation.reversal . Perm.Operation.complement) p
[3,1,4,2,5]
λ: (Perm.Operation.complement . Perm.Operation.reversal) p
[3,1,4,2,5]
λ: Perm.Operation.inverse p
[1,3,5,2,4]
```

### Basic statistics

An *ascent* (resp. *descent*) in a permutation is an entry followed by a
larger (resp. smaller) entry.
An entry of a permutation which is smaller (resp. larger) than
all the entries that precede it is called a *left-to-right minimum*
(resp. *left-to-right maximum*).
An entry of a permutation which is smaller (resp. larger) than
all the entries that follow it is called a *right-to-left minimum*
(resp. *right-to-left maximum*).

```haskell
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: import qualified Data.Algorithm.PPattern.Perm.Statistics as Perm.Statistics
λ: let p = Perm.mk [7,5,3,8,2,1,4,9,6]
λ: Perm.Statistics.leftToRightMinima p
[Point (1,7),Point (2,5),Point (3,3),Point (5,2),Point (6,1)]
λ: Perm.Statistics.leftToRightMaxima p
[7,8,9]
λ: Perm.Statistics.leftToRightMaxima p
[Point (1,7),Point (4,8),Point (8,9)]
λ: Perm.Statistics.rightToLeftMaxima p
[Point (8,9),Point (9,6)]
λ: Perm.Statistics.ascents p
[Point (3,3),Point (6,1),Point (7,4)]
λ: Perm.Statistics.doubleAscents p
[Point (6,1)]
λ: Perm.Statistics.descents p
[Point (1,7),Point (2,5),Point (4,8),Point (5,2),Point (8,9)]
λ: Perm.Statistics.doubleDescents p
[Point (1,7),Point (4,8)]
λ: Perm.Statistics.peaks p
[Point (4,8),Point (8,9)]
λ: Perm.Statistics.valleys p
[Point (3,3),Point (6,1)]
```

## Pattern matching

### Basic pattern matching

+ Positive search:

```haskell
λ: import qualified Data.Algorithm.PPattern as PPattern
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: let p = Perm.mk [2,1,3]
λ: let q = Perm.mk [6,2,3,5,1,4]
λ: PPattern.search p q
Just [(Point (1,2),Point (2,2)),(Point (2,1),Point (5,1)),(Point (3,3),Point (6,4))]
λ: p `PPattern.occursIn` q
True
λ: q `PPattern.avoids` p
False
```

+ Negative search:

```haskell
λ: import qualified Data.Algorithm.PPattern as PPattern
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: let p = Perm.mk [1..3]
λ: let q = Perm.mk [6..1]
λ: PPattern.search p q
Nothing
λ: p `PPattern.occursIn` q
False
λ: q `PPattern.avoids` p
True
```

### Occurrence

The `Data.Algorithm.PPattern.search` function has type
```haskell
import qualified Data.Algorithm.PPattern.Occurrence as Occurrence
import qualified Data.Algorithm.PPattern.Perm as Perm
search :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
```
as shown in
```haskell
λ: import qualified Data.Algorithm.PPattern as PPattern
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: let p = Perm.mk [2,1,3]
λ: let q = Perm.mk [6,2,3,5,1,4]
λ: let o = PPattern.search p q
λ: :type o
o :: Maybe Data.Algorithm.PPattern.Occurrence.Occurrence
```

The `Data.Algorithm.PPattern.Occurrence` module provides several function
for querying `Data.Algorithm.PPattern.Occurrence.Occurrence` type variables.


```haskell
λ: import Data.Maybe
λ: import qualified Data.Algorithm.PPattern.Occurrence as Occurrence
λ: Occurrence.size (fromJust o)
3
λ: Occurrence.pattern (fromJust o)
[2,1,3]
λ: Occurrence.patternPoints (fromJust o)
[Point (1,2),Point (2,1),Point (3,3)]
λ: Occurrence.target (fromJust o)
[2,1,3]
λ: Occurrence.targetPoints (fromJust o)
[Point (2,2),Point (5,1),Point (6,4)]
```

### Strategy

+ *leftmost conflict* (`Data.Algorithm.PPattern.Strategy.leftmostConflict`)

+ *leftmost order conflict* (`Data.Algorithm.PPattern.Strategy.leftmostOrderConflictFirst`)

+ *leftmost value conflict* (`Data.Algorithm.PPattern.Strategy.leftmostValueConflictFirst`)

+ *rightmost conflict* (`Data.Algorithm.PPattern.Strategy.rightmostConflict`)

+ *rightmost order conflict* (`Data.Algorithm.PPattern.Strategy.rightmostOrderConflictFirst`)

+ *rightmost value conflict* (`Data.Algorithm.PPattern.Strategy.rightmostValueConflictFirst`)

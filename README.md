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

The *reverse* of a permutation <img alt="$\sigma = \sigma_1 \sigma_1 \ldots \sigma_n$" src="svgs/f00e73d0681e93ba274aa2e29bda2bb0.png?invert_in_darkmode" align=middle width="107.393715pt" height="14.102549999999994pt"/>
is the permutation <img alt="$\sigma_n \sigma_{n-1} \ldots \sigma_1$" src="svgs/136707a3c7df490ad43d3008f02c3739.png?invert_in_darkmode" align=middle width="93.981855pt" height="14.102549999999994pt"/>.
The *complement* of <img alt="$\sigma$" src="svgs/8cda31ed38c6d59d14ebefa440099572.png?invert_in_darkmode" align=middle width="9.945705000000002pt" height="14.102549999999994pt"/> is the permutation
<img alt="$\beta_1 \beta_2 \ldots \beta_n$" src="svgs/69c5ec9825b74db00546c7649d8fcb9f.png?invert_in_darkmode" align=middle width="75.291645pt" height="22.745910000000016pt"/>, where
<img alt="$\beta_i = n+1-\sigma_i$" src="svgs/859e546341232bcfdca716001b2e56df.png?invert_in_darkmode" align=middle width="108.70282499999999pt" height="22.745910000000016pt"/>.
That is, the complement substitutes the largest element of a permutation
by the smallest one, the next largest element by the next smallest element, etc.
The *inverse* of <img alt="$\sigma$" src="svgs/8cda31ed38c6d59d14ebefa440099572.png?invert_in_darkmode" align=middle width="9.945705000000002pt" height="14.102549999999994pt"/> is the permutation where the <img alt="$\sigma_i$" src="svgs/e61ae7f2cb94c8418c30517775fde77d.png?invert_in_darkmode" align=middle width="13.991505000000002pt" height="14.102549999999994pt"/>-th position
is occupied by <img alt="$i$" src="svgs/77a3b857d53fb44e33b53e4c8b68351a.png?invert_in_darkmode" align=middle width="5.642109000000004pt" height="21.602129999999985pt"/>.

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

### Strategy

+ *leftmost conflict* (`Data.Algorithm.PPattern.Strategy.leftmostConflict`)

+ *leftmost order conflict* (`Data.Algorithm.PPattern.Strategy.leftmostOrderConflictFirst`)

+ *leftmost value conflict* (`Data.Algorithm.PPattern.Strategy.leftmostValueConflictFirst`)

+ *rightmost conflict* (`Data.Algorithm.PPattern.Strategy.rightmostConflict`)

+ *rightmost order conflict* (`Data.Algorithm.PPattern.Strategy.rightmostOrderConflictFirst`)

+ *rightmost value conflict* (`Data.Algorithm.PPattern.Strategy.rightmostValueConflictFirst`)

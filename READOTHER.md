<!-- python -m readme2tex --usepackage "tikz" --usepackage "xcolor" --output README.md --readme  READOTHER.md --nocdn --pngtrick -->

# PPattern :

## APermutations

### Implementation

```haskell
-- Define in Data.Algorithm.PPattern.Geometry.Point.hs
newtype Point = Point (Int, Int) deriving (Show, Eq, Ord)

-- Define in Data.Algorithm.PPattern.Geometry.APoint.hs
newtype APoint a = APoint (Point, a) deriving (Eq, Ord, Show)

-- Define in Data.Algorithm.PPattern.APerm.hs
newtype APerm a = APerm { getList :: [APoint a] } deriving (Eq, Ord)
```

```haskell
mk :: (Foldable t, Ord a) => t a -> APerm a
mk = APerm . fmap (uncurry APerm.T.mk) . reduce . Foldable.toList
```

### Basic manipulation

```haskell
λ: import qualified Data.Algorithm.PPattern.APerm as APerm
λ: let p = APerm.mk "acedb"
λ: p
[1,3,5,4,2]
λ: APerm.annotations p
"acedb"
λ: APerm.toList p
[T (Point (1,1),'a'),T (Point (2,3),'c'),T (Point (3,5),'e'),T (Point (4,4),'d'),T (Point (5,2),'b')]
λ: APerm.toPoints p
[Point (1,1),Point (2,3),Point (3,5),Point (4,4),Point (5,2)]
λ: APerm.xCoords p
[1,2,3,4,5]
λ: APerm.yCoords p
[1,3,5,4,2]
λ:
```

As you might have guessed, `show`reduces to `yCoords`:

```haskell
instance Show (APerm a) where
  show = show . yCoords
```

### Basic properties


### Ties

```haskell
λ: import qualified Data.Algorithm.PPattern.APerm as APerm
λ: let p = APerm.mk "ababc"
λ: p
[1,3,2,4,5]
λ:
```

### Transforming APermutations

The *reverse* of a APermutation $\sigma = \sigma_1 \sigma_2 \ldots \sigma_n$
is the APermutation $r(\sigma) = \sigma_n \sigma_{n-1} \ldots \sigma_1$.
The *complement* $c(\sigma)$ of $\sigma$ is the APermutation
$\sigma_1' \sigma_2' \ldots \sigma_n'$ where
$\sigma_i' = n+1-\sigma_i$.
The *inverse* is the regular group theoretical inverse on APermutations;
that is, the $\sigma-i$-th position of the inverse $\sigma^{-1}$ is occupied by
$i$.
Going back to the library,
`reversal`, `complement` and `inverse` yield the reverse, complement and
inverse operations, respectivelly.
`reversalComplement` is the composition of the complement and reverse
(he complement is applied first).

```haskell
λ: import qualified Data.Algorithm.PPattern.APerm as APerm
λ: let p = APerm.mk [3,5,7,1,8,4,2,6]
λ: APerm.reversal p
[6,2,4,8,1,7,5,3]
λ: APerm.complement p
[6,4,2,8,1,5,7,3]
λ: APerm.reversalComplement p
[3,7,5,1,8,2,4,6]
λ: APerm.inverse p
[4,7,1,6,2,8,3,5]
λ:
```

### Composing APermutations

#### Sums

The *skew sum* and *direct sum* of APermutations are two operations
to combine shorter APermutations into longer ones. Given a APermutation $\pi$
of length $m$ and the APermutation $\sigma$ of length $n$,
the skew sum of $\pi$ and $\sigma$ is the APermutation of length $m + n$ defined by
$$
(\pi \ominus \sigma )(i)=
\begin{cases}
  \pi (i)+n    & \text{for } 1\leq i\leq m,\\
  \sigma (i-m) & \text{for } m+1\leq i\leq m+n,
\end{cases}
$$

and the direct sum of $\pi$ and $\sigma$ is the APermutation of length $m + n$ defined by
$$
(\pi \oplus \sigma )(i)=
\begin{cases}
  \pi (i)          & \text{for } 1\leq i\leq m,\\
  \sigma (i-m) + m & \text{for } m+1\leq i\leq m+n.
\end{cases}
$$

```haskell
λ: import qualified Data.Algorithm.PPattern.APerm as APerm
λ: import qualified Data.Algorithm.PPattern.APerm.Sum as APerm.Sum
λ: let p = APerm.mk [2,4,1,3]
λ: let q = APerm.mk [3,5,1,4,2]
λ: APerm.Sum.skewSum p q
[7,9,6,8,3,5,1,4,2]
λ: APerm.Sum.directSum p q
[2,4,1,3,7,9,5,8,6]
λ:
```

### Basic statistics

```haskell
λ: import qualified Data.Algorithm.PPattern.APerm as APerm
λ: import qualified Data.Algorithm.PPattern.APerm.Statistics as APerm.Statistics
λ: let p = APerm.mk [7,5,3,8,2,1,4,9,6]
λ: APerm.Statistics.leftToRightMinima p
[7,5,3,2,1]
λ: APerm.Statistics.leftToRightMaxima p
[7,8,9]
λ: APerm.Statistics.rightToLeftMinima p
[1,4,6]
λ: APerm.Statistics.rightToLeftMaxima p
[9,6]
λ: APerm.Statistics.ascents p
[3,1,4]
λ: APerm.Statistics.doubleAscents p
[1]
λ: APerm.Statistics.descents p
[7,5,8,2,9]
λ: APerm.Statistics.doubleDescents p
[7,8]
λ: APerm.Statistics.peaks p
[8,9]
λ: APerm.Statistics.valleys p
[3,1]
λ:
```

### APermutation graphs

## Pattern matching

### Generic pattern matching

### Monotone patterns

### Separable patterns

### Size-3 patterns

### Size-4 patterns

## APermutation classes

### Separable APermutations

A separable APermutation is a APermutation that can be obtained from the trivial
APermutation 1 by direct sums and skew sums;
separable APermutations may be characterized by the forbidden APermutation patterns
2413 and 3142
(see <https://en.wikipedia.org/wiki/Separable_APermutation>).

```haskell
λ: import qualified Data.Algorithm.PPattern.APerm as APerm
λ: import qualified Data.Algorithm.PPattern.SeparatingTree as SeparatingTree
λ: let p = APerm.mk [3,1,4,2,6,5,7]
λ: -- p does not avoid 2413 and 3142, and hence is not separable
λ: APerm.isSeparable p                  
False
λ: -- so that no separating tree of p can be obtained
λ: SeparatingTree.mk <img alt="$ APerm.toPoints p&#10;Nothing&#10;λ: let q = APerm.mk [3,1,2,4,6,5,7]&#10;λ: APerm.isSeparable q                             -- q does avoid 2413 and 3142, and hence is separable&#10;True&#10;λ: import Data.Maybe&#10;λ: fromJust . SeparatingTree.mk $" src="https://rawgit.com/in	git@github.com:vialette/ppattern/None/svgs/c7be5bdfada4253d65cf69b07d4cdacb.svg?invert_in_darkmode" align=middle width="1011.5407499999999pt" height="45.82083000000002pt"/> APerm.toPoints q -- so that a separating tree of q can be obtained
+ Interval (1,7)
.+ Interval (1,6)
..+ Interval (1,4)
...- Interval (1,3)
....Point (1,3)
....+ Interval (1,2)
.....Point (2,1)
.....Point (3,2)
...Point (4,4)
..- Interval (5,6)
...Point (5,6)
...Point (6,5)
.Point (7,7)

λ:
```

### (213,231)-avoiding patterns

## Computing a base

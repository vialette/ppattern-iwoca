# PPattern :

## Permutations

### Basic manipulation

```haskell
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: let p = Perm.mk "acedb"
λ: p
[1,3,5,4,2]
λ: Perm.annotations p
"acedb"
λ: Perm.toList p
[T (Point (1,1),'a'),T (Point (2,3),'c'),T (Point (3,5),'e'),T (Point (4,4),'d'),T (Point (5,2),'b')]
λ: Perm.toPoints p
[Point (1,1),Point (2,3),Point (3,5),Point (4,4),Point (5,2)]
λ: Perm.xCoords p
[1,2,3,4,5]
λ: Perm.yCoords p
[1,3,5,4,2]
λ:
```

As you might have guessed, `show`reduces to `yCoords`:

```haskell
instance Show (Perm a) where
  show = show . yCoords
```

### Basic properties


### Ties

```haskell
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: let p = Perm.mk "ababc"
λ: p
[1,3,2,4,5]
λ:
```

### Transforming permutations

The *reverse* of a permutation $\sigma = \sigma_1 \sigma_2 \ldots \sigma_n$
is the permutation $r(\sigma) = \sigma_n \sigma_{n-1} \ldots \sigma_1$.
The *complement* $c(\sigma)$ of $\sigma$ is the permutation
$\sigma_1' \sigma_2' \ldots \sigma_n'$ where
$\sigma_i' = n+1-\sigma_i$.
The *inverse* is the regular group theoretical inverse on permutations;
that is, the $\sigma-i$-th position of the inverse $\sigma^{-1}$ is occupied by
$i$.

```haskell
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: let p = Perm.mk [3,5,7,1,8,4,2,6]
λ: Perm.reversal p
[6,2,4,8,1,7,5,3]
λ: Perm.complement p
[6,4,2,8,1,5,7,3]
λ: Perm.reversalComplement p
[3,7,5,1,8,2,4,6]
λ: Perm.inverse p
[4,7,1,6,2,8,3,5]
λ:
```

### Composing permutations

#### Sums

The *skew sum* and *direct sum* of permutations are two operations
to combine shorter permutations into longer ones. Given a permutation $\pi$
of length $m$ and the permutation $\sigma$ of length $n$,
the skew sum of $\pi$ and $\sigma$ is the permutation of length $m + n$ defined by
$$
(\pi \ominus \sigma )(i)=
\begin{cases}
  \pi (i)+n    & \text{for } 1\leq i\leq m,\\
  \sigma (i-m) & \text{for } m+1\leq i\leq m+n,
\end{cases}
$$

and the direct sum of $\pi$ and $\sigma$ is the permutation of length $m + n$ defined by
$$
(\pi \oplus \sigma )(i)=
\begin{cases}
  \pi (i)          & \text{for } 1\leq i\leq m,\\
  \sigma (i-m) + m & \text{for } m+1\leq i\leq m+n.
\end{cases}
$$

```haskell
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: import qualified Data.Algorithm.PPattern.Perm.Sum as Perm.Sum
λ: let p = Perm.mk [2,4,1,3]
λ: let q = Perm.mk [3,5,1,4,2]
λ: Perm.Sum.skewSum p q
[7,9,6,8,3,5,1,4,2]
λ: Perm.Sum.directSum p q
[2,4,1,3,7,9,5,8,6]
λ:
```

### Basic statistics

```haskell
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: import qualified Data.Algorithm.PPattern.Perm.Statistics as Perm.Statistics
λ: let p = Perm.mk [7,5,3,8,2,1,4,9,6]
λ: Perm.Statistics.leftToRightMinima p
[7,5,3,2,1]
λ: Perm.Statistics.leftToRightMaxima p
[7,8,9]
λ: Perm.Statistics.rightToLeftMinima p
[1,4,6]
λ: Perm.Statistics.rightToLeftMaxima p
[9,6]
λ: Perm.Statistics.ascents p
[3,1,4]
λ: Perm.Statistics.doubleAscents p
[1]
λ: Perm.Statistics.descents p
[7,5,8,2,9]
λ: Perm.Statistics.doubleDescents p
[7,8]
λ: Perm.Statistics.peaks p
[8,9]
λ: Perm.Statistics.valleys p
[3,1]
λ:
```

### Permutation graphs

## Pattern matching

### Generic pattern matching

### Monotone patterns

### Separable patterns

### Size-3 patterns

### Size-4 patterns

## Permutation classes

### Separable permutations

A separable permutation is a permutation that can be obtained from the trivial
permutation 1 by direct sums and skew sums;
separable permutations may be characterized by the forbidden permutation patterns
2413 and 3142
(see <https://en.wikipedia.org/wiki/Separable_permutation>).

```haskell
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: import qualified Data.Algorithm.PPattern.SeparatingTree as SeparatingTree
λ: let p = Perm.mk [3,1,4,2,6,5,7]
λ: -- p does not avoid 2413 and 3142, and hence is not separable
λ: Perm.isSeparable p                  
False
λ: -- so that no separating tree of p can be obtained
λ: SeparatingTree.mk <img alt="$ Perm.toPoints p&#10;Nothing&#10;λ: let q = Perm.mk [3,1,2,4,6,5,7]&#10;λ: Perm.isSeparable q                             -- q does avoid 2413 and 3142, and hence is separable&#10;True&#10;λ: import Data.Maybe&#10;λ: fromJust . SeparatingTree.mk $" src="https://rawgit.com/in	git@github.com:vialette/ppattern/None/svgs/c7be5bdfada4253d65cf69b07d4cdacb.svg?invert_in_darkmode" align=middle width="1011.5407499999999pt" height="45.82083000000002pt"/> Perm.toPoints q -- so that a separating tree of q can be obtained
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

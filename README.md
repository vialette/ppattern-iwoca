# PPattern :

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

### Separable Permutations

A separable Permutation is a Permutation that can be obtained from the trivial
Permutation 1 by direct sums and skew sums;
separable Permutations may be characterized by the forbidden Permutation patterns
2413 and 3142
(see <https://en.wikipedia.org/wiki/Separable_Permutation>).

```haskell
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: import qualified Data.Algorithm.PPattern.SeparatingTree as SeparatingTree
λ: let p = Perm.mk [3,1,4,2,6,5,7]
λ: -- p does not avoid 2413 and 3142, and hence is not separable
λ: Perm.isSeparable p                  
False
λ: -- so that no separating tree of p can be obtained
λ: SeparatingTree.mk <img alt="<img alt="$ Perm.toPoints p&amp;#10;Nothing&amp;#10;λ: let q = Perm.mk [3,1,2,4,6,5,7]&amp;#10;λ: Perm.isSeparable q                             -- q does avoid 2413 and 3142, and hence is separable&amp;#10;True&amp;#10;λ: import Data.Maybe&amp;#10;λ: fromJust . SeparatingTree.mk $" src="svgs/6cf3447b0450166911e817c41625f463.png?invert_in_darkmode" align=middle width="841.26405pt" height="45.82083000000002pt"/>" src="https://rawgit.com/in	git@github.com:vialette/ppattern/None/svgs/c7be5bdfada4253d65cf69b07d4cdacb.svg?invert_in_darkmode" align=middle width="1011.5407499999999pt" height="45.82083000000002pt"/> Perm.toPoints q -- so that a separating tree of q can be obtained
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

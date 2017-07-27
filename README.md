<!-- python -m readme2tex --usepackage "tikz" --usepackage "xcolor" --output README.md --readme  READOTHER.md --nocdn --pngtrick -->

# PPattern - Permutation Pattern

A permutation of length <img alt="$n$" src="svgs/55a049b8f161ae7cfeb0197d75aff967.png?invert_in_darkmode" align=middle width="9.830040000000002pt" height="14.102549999999994pt"/> is a bijective mapping
<img alt="$\tau : [n] \to [n]$" src="svgs/718a3cb0ff45d3938ea7f92402603e09.png?invert_in_darkmode" align=middle width="86.06729999999999pt" height="24.56552999999997pt"/>; one way to represent it is as the
sequence of numbers <img alt="$\tau[1] \tau[2] \ldots \tau[n]$" src="svgs/54b58f0ce8ad4d9440c4fe6a3eb0de9b.png?invert_in_darkmode" align=middle width="105.21934499999999pt" height="24.56552999999997pt"/>.
A permutation <img alt="$\tau$" src="svgs/0fe1677705e987cac4f589ed600aa6b3.png?invert_in_darkmode" align=middle width="9.013125000000002pt" height="14.102549999999994pt"/> *contains* permutation <img alt="$\pi$" src="svgs/f30fdded685c83b0e7b446aa9c9aa120.png?invert_in_darkmode" align=middle width="9.922935000000003pt" height="14.102549999999994pt"/> if <img alt="$\pi$" src="svgs/f30fdded685c83b0e7b446aa9c9aa120.png?invert_in_darkmode" align=middle width="9.922935000000003pt" height="14.102549999999994pt"/>
has a (not necessarily consecutive) subsequence where the relative
ordering of the elements is the same as in <img alt="$\tau$" src="svgs/0fe1677705e987cac4f589ed600aa6b3.png?invert_in_darkmode" align=middle width="9.013125000000002pt" height="14.102549999999994pt"/>.
In this case, <img alt="$\pi$" src="svgs/f30fdded685c83b0e7b446aa9c9aa120.png?invert_in_darkmode" align=middle width="9.922935000000003pt" height="14.102549999999994pt"/> is a *subpattern* of <img alt="$\tau$" src="svgs/0fe1677705e987cac4f589ed600aa6b3.png?invert_in_darkmode" align=middle width="9.013125000000002pt" height="14.102549999999994pt"/>;
otherwise, <img alt="$\tau$" src="svgs/0fe1677705e987cac4f589ed600aa6b3.png?invert_in_darkmode" align=middle width="9.013125000000002pt" height="14.102549999999994pt"/> avoids <img alt="$\pi$" src="svgs/f30fdded685c83b0e7b446aa9c9aa120.png?invert_in_darkmode" align=middle width="9.922935000000003pt" height="14.102549999999994pt"/>.
For example, <img alt="$3215674$" src="svgs/e34e4704dc5af80561a82d0bacc7d14f.png?invert_in_darkmode" align=middle width="57.319514999999996pt" height="21.10812pt"/> contains the pattern <img alt="$132$" src="svgs/acdbd4a445cb5214dab41fc6c15bc38d.png?invert_in_darkmode" align=middle width="24.56553pt" height="21.10812pt"/>, since the subsequence
<img alt="$154$" src="svgs/84c9edc740a30b19a1fa77d214677eb0.png?invert_in_darkmode" align=middle width="24.56553pt" height="21.10812pt"/> is ordered the same way as <img alt="$132$" src="svgs/acdbd4a445cb5214dab41fc6c15bc38d.png?invert_in_darkmode" align=middle width="24.56553pt" height="21.10812pt"/>.
On the other hand, the permutation avoids <img alt="$4321$" src="svgs/62e593fc80b2b2300c678e7eb5e38b15.png?invert_in_darkmode" align=middle width="32.753985pt" height="21.10812pt"/>: it does
not contain a descending subsequence of 4 elements.

Given permutation <img alt="$\pi$" src="svgs/f30fdded685c83b0e7b446aa9c9aa120.png?invert_in_darkmode" align=middle width="9.922935000000003pt" height="14.102549999999994pt"/> and <img alt="$\tau$" src="svgs/0fe1677705e987cac4f589ed600aa6b3.png?invert_in_darkmode" align=middle width="9.013125000000002pt" height="14.102549999999994pt"/>, the *Permutation Pattern* problem
is to decide if <img alt="$\tau$" src="svgs/0fe1677705e987cac4f589ed600aa6b3.png?invert_in_darkmode" align=middle width="9.013125000000002pt" height="14.102549999999994pt"/> contains <img alt="$\pi$" src="svgs/f30fdded685c83b0e7b446aa9c9aa120.png?invert_in_darkmode" align=middle width="9.922935000000003pt" height="14.102549999999994pt"/>.
The *Permutation Pattern* problem is NP-complete.
It can be solved by brute force in time <img alt="$O(n^k)$" src="svgs/7872efdd46393cf2267348a2b61e11c6.png?invert_in_darkmode" align=middle width="43.604220000000005pt" height="27.852989999999977pt"/>, where
<img alt="$n = |\tau|$" src="svgs/1b86d7172a06cda0f0649dc6df40d4f4.png?invert_in_darkmode" align=middle width="49.818285pt" height="24.56552999999997pt"/> and <img alt="$k = |\pi|$" src="svgs/aef90dd61609593d79e73448d778732e.png?invert_in_darkmode" align=middle width="49.936755000000005pt" height="24.56552999999997pt"/>.
This has been improved to <img alt="$O(n^{0.47k + o(k)})$" src="svgs/d70eeed1ab6baec80fc692f1fb94188a.png?invert_in_darkmode" align=middle width="101.28591pt" height="29.12679000000001pt"/>
by Ahal and Rabinovich.
Guillemot and Marx proved that the *Permutation Pattern* problem
can be solved in time <img alt="$2^{O(k^2 \log(k))}}$" src="svgs/26b858e8e4cd93d52f257ce3fd5f4cd6.png?invert_in_darkmode" align=middle width="79.11898500000001pt" height="32.40632999999999pt"/>
(i.e., the *Permutation Pattern* problem is fixed-parameter tractable
parameterized by the size of the pattern).

## Permutations

### Implementation

Permutations are implemented as lists of points with increasing x-coordinates.

```haskell
-- Define in Data.Algorithm.PPattern.Geometry.Point.hs
newtype Point = Point (Int, Int) deriving (Show, Eq, Ord)

-- Define in Data.Algorithm.PPattern.Perm.hs
newtype Perm = Perm { getList :: [Point] } deriving (Eq, Ord)
```

The function `Data.Algorithm.PPattern.Perm.mk :: (Foldable t, Ord a) => t a -> Perm`
is devoted to creating permutations from foldable objects.

```haskell
λ: Perm.mk [2,1,3]
[2,1,3]
λ: Perm.mk "bac"
[2,1,3]
λ: Perm.mk ["tomorrow", "today", "yesterday"]
[2,1,3]
λ: Perm.mk "bac" == Perm.mk ["tomorrow", "today", "yesterday"]
True
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
λ: mapM_ print [Perm.pointAtXCoord x p | x <- [0..6]]
Nothing
Just (Point (1,1))
Just (Point (2,3))
Just (Point (3,5))
Just (Point (4,4))
Just (Point (5,2))
Nothing
λ: mapM_ print [Perm.pointAtYCoord y p | y <- [0..6]]
Nothing
Just (Point (1,1))
Just (Point (5,2))
Just (Point (2,3))
Just (Point (4,4))
Just (Point (3,5))
Nothing
```

Notice that `Data.Algorithm.PPattern.Perm.xCoords` and
`Data.Algorithm.PPattern.Perm.yCoords` are reduced forms for:

```haskell
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
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

Use `Data.Algorithm.PPattern.Perm.mkSafe` to forbid ties.

```haskell
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: Perm.mkSafe "acb"
Just [1,3,2]
λ: Perm.mkSafe "acbacb"
Nothing
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

An *ascent* (resp. *descent*) in a permutation is an entry followed by a
larger (resp. smaller) entry.
An entry of a permutation which is smaller (resp. larger) than
all the entries that precede it is called a *left-to-right minimum*
(resp. *left-to-right maximum*).
An entry of a permutation which is smaller (resp. larger) than
all the entries that follow it is called a *right-to-left minimum*
(resp. *right-to-left maximum*).
A *valley* (resp. *peak*) in a permutation is an entry that is smaller (resp. larger)
than both of its neighbors.

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

The `Data.Algorithm.PPattern.search` function is for searching the occurrence of
a pattern (short permutation) in a larger permutatins.
The result (in case of succeed) is given in the form of a point to point mapping.
The `Data.Algorithm.PPattern.occursIn`,
`Data.Algorithm.PPattern.avoids` and
`Data.Algorithm.PPattern.contains` functions discard the solution mapping and
return a boolean.

+ Positive search:

```haskell
λ: import qualified Data.Algorithm.PPattern as PPattern
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: let p = Perm.mk [3,2,4,1]
λ: let q = Perm.mk [5,4,3,6,2,1]
λ: PPattern.search p q
Just Point (1,3) -> Point (2,4), Point (2,2) -> Point (3,3), Point (3,4) -> Point (4,6), Point (4,1) -> Point (6,1)
λ: p `PPattern.occursIn` q
True
λ: q `PPattern.contains` p
True
λ: q `PPattern.avoids` p
False
```

+ Negative search:

```haskell
λ: import qualified Data.Algorithm.PPattern as PPattern
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: let p = Perm.mk [1,2,3]
λ: let q = Perm.mk [5,4,3,6,2,1]
λ: PPattern.search p q
Nothing
λ: p `PPattern.occursIn` q
False
λ: q `PPattern.contains` p
True
λ: q `PPattern.avoids` p
True

```

### Occurrence

The `Data.Algorithm.PPattern.search` function has type
```haskell
import qualified Data.Algorithm.PPattern.Perm as Perm
import qualified Data.Algorithm.PPattern.Search.Occurrence as Occurrence
search :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
```

The `Data.Algorithm.PPattern.Occurrence` module provides several functions
for querying `Data.Algorithm.PPattern.Occurrence.Occurrence` type variables.

```haskell
λ: import qualified Data.Maybe as Maybe
λ: import qualified Data.Algorithm.PPattern as PPattern
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: import qualified Data.Algorithm.PPattern.Search.Occurrence as Occurrence
λ: let q = Perm.mk [5,4,3,6,2,1]
λ: let p = Perm.mk [3,2,4,1]
λ: let o = PPattern.search p q
λ: Occurrence.size (Maybe.fromJust o)
4
λ: Occurrence.pattern (Maybe.fromJust o)
[3,2,4,1]
λ: Occurrence.patternPoints (Maybe.fromJust o)
[Point (1,3),Point (2,2),Point (3,4),Point (4,1)]
λ: Occurrence.target (Maybe.fromJust o)
[3,2,4,1]
λ: Occurrence.targetPoints (Maybe.fromJust o)
[Point (2,4),Point (3,3),Point (4,6),Point (6,1)]
```

Of course, the following assertions always hold.

```haskell
λ: Maybe.isNothing o || p == Occurrence.pattern (Maybe.fromJust o)
True
λ: Maybe.isNothing o || p == Occurrence.target (Maybe.fromJust o)
True
```

### Resolving conflicts

Conflict resolution is a key part of the approach.
Given two permutations, <img alt="$\pi$" src="svgs/f30fdded685c83b0e7b446aa9c9aa120.png?invert_in_darkmode" align=middle width="9.922935000000003pt" height="14.102549999999994pt"/> of <img alt="$[m]$" src="svgs/dd493f02a0fe43f96fae4e8995770011.png?invert_in_darkmode" align=middle width="23.477520000000005pt" height="24.56552999999997pt"/> and <img alt="$\tau$" src="svgs/0fe1677705e987cac4f589ed600aa6b3.png?invert_in_darkmode" align=middle width="9.013125000000002pt" height="14.102549999999994pt"/> of <img alt="$[n]$" src="svgs/e56c228e9cc317db54aad972ab7f99e9.png?invert_in_darkmode" align=middle width="18.92847pt" height="24.56552999999997pt"/>, a *pre-embedding*
of <img alt="$\pi$" src="svgs/f30fdded685c83b0e7b446aa9c9aa120.png?invert_in_darkmode" align=middle width="9.922935000000003pt" height="14.102549999999994pt"/> into <img alt="$\tau$" src="svgs/0fe1677705e987cac4f589ed600aa6b3.png?invert_in_darkmode" align=middle width="9.013125000000002pt" height="14.102549999999994pt"/> is any function <img alt="$e: [m] \to [n]$" src="svgs/7608a5022962af6c21393a6adcfc871c.png?invert_in_darkmode" align=middle width="89.221935pt" height="24.56552999999997pt"/>.
A pair <img alt="$(i, j)$" src="svgs/e8873e227619b7a62ee7eb981ef1faea.png?invert_in_darkmode" align=middle width="33.35376pt" height="24.56552999999997pt"/> with <img alt="$i &lt; j$" src="svgs/2e8f6bc2b2b5781d1b52d17c82893806.png?invert_in_darkmode" align=middle width="35.19351pt" height="21.602129999999985pt"/> is a

+ *horizontal* conflict for <img alt="$e$" src="svgs/8cd34385ed61aca950a6b06d09fb50ac.png?invert_in_darkmode" align=middle width="7.625590500000003pt" height="14.102549999999994pt"/> if <img alt="$e(i) &gt; e(j)$" src="svgs/45a5f584f6e662a77edd49e8adbf653f.png?invert_in_darkmode" align=middle width="75.92359499999999pt" height="24.56552999999997pt"/>,

+ *vertical* conflict for <img alt="$e$" src="svgs/8cd34385ed61aca950a6b06d09fb50ac.png?invert_in_darkmode" align=middle width="7.625590500000003pt" height="14.102549999999994pt"/> if <img alt="$\pi[i] &lt; \pi[j]$" src="svgs/05ac3568c0df484dd1702372599024d0.png?invert_in_darkmode" align=middle width="73.243995pt" height="24.56552999999997pt"/> and <img alt="$\tau[e(i)] &gt; \tau[e(j)]$" src="svgs/a1f26a2237ccd2f6c5d9a0e6d984d790.png?invert_in_darkmode" align=middle width="112.16023499999999pt" height="24.56552999999997pt"/>,
or <img alt="$\pi[i] &gt; \pi[j]$" src="svgs/91663a7b958d164af745e39f1f7fc2dc.png?invert_in_darkmode" align=middle width="73.243995pt" height="24.56552999999997pt"/> and <img alt="$\tau[e(i)] &lt;\tau[e(j)]$" src="svgs/b23f5ca1825ed281e68e80013b982669.png?invert_in_darkmode" align=middle width="112.16023499999999pt" height="24.56552999999997pt"/>.

The algorithm used by `PPattern` is to recursively find and resolve conflicts.
If a conflict cannot be resolved then there is no occurrence of the pattern in
the permutation.
Several conflict selection strategies are conceivable.
The following conflict selection strategies are implemented within `PPattern`.

+ *leftmost conflict first* (`Data.Algorithm.PPattern.Search.ConflictSelection.LeftmostConflictFirst`):
Resolve - if possible - any leftmost conflict first
(this conflict is either a horizontal or a vertical conflict).

+ *leftmost horizontal conflict* (`Data.Algorithm.PPattern.Search.ConflictSelection.LeftmostHorizontalConflictFirst`):
Resolve - if possible - the leftmost horizontal conflict first.
If such a conflict does not not exist,
resolve - if possible -  the leftmost vertical conflict first.

+ *leftmost vertical conflict* (`Data.Algorithm.PPattern.Search.ConflictSelection.LeftmostVerticalConflictFirst`):
Resolve - if possible - the leftmost vertical conflict first.
If such a conflict does not not exist,
resolve - if possible - the leftmost horizontal conflict first.

+ *rightmost conflict first* (`Data.Algorithm.PPattern.Search.ConflictSelection.RightmostConflictFirst`):
Resolve - if possible - any rightmost conflict first
(this conflict is either a horizontal or a vertical conflict).

+ *rightmost horizontal conflict* (`Data.Algorithm.PPattern.Search.ConflictSelection.RightmostHorizontalConflictFirst`):
Resolve - if possible - the rightmost horizontal conflict first.
If such a conflict does not not exist,
resolve - if possible - the rightmost vertical conflict first.

+ *rightmost vertical conflict* (`Data.Algorithm.PPattern.Search.ConflictSelection.RighmostVerticalConflictFirst`):
Resolve - if possible - the rightmost vertical conflict first.
If such a conflict does not not exist,
resolve - if possible - the rightmost horizontal conflict first.

As of version 0.1.0.0 of `PPattern`,
the `Data.Algorithm.PPattern.search` function uses a default
leftmost horizontal conflict first resolution algorithm.
The `Data.Algorithm.PPattern` module offers the following search functions for using
a specific conflict resolution algorithm.

```haskell
import qualified Data.Algorithm.PPattern.Perm as Perm
import qualified Data.Algorithm.PPattern.Search.Occurrence as Occurrence

searchLeftmostConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
searchLeftmostHorizontalConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
searchLeftmostVerticalConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
searchRightmostConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
searchRightmostHorizontalConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
searchRightmostVerticalConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
```

The `Data.Algorithm.PPattern.searchWithConflictSelectionStrategy` allows for
new implemented conflict selection strategies.

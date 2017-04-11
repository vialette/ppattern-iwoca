# PPattern :

## Permutations

### Basic manipulation

### Basic properties

### Transforming permutations

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

##### Skew-sum

##### Direct-sum


### Basic statistics

### Permutation graphs

## Pattern matching

### Generic pattern matching

### Monotone patterns

### Separable patterns

### Size-3 patterns

### Size-4 patterns

## Permutation classes

### Separable permutations

```haskell
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: import qualified Data.Algorithm.PPattern.SeparatingTree as SeparatingTree
λ: let p = Perm.mk [3,1,4,2,6,5,7]
λ: -- p does not avoid 2413 and 3142, and hence is not separable
λ: Perm.isSeparable p                  
False
λ: -- so that no separating tree of p can be obtained
λ: SeparatingTree.mk $ Perm.toPoints p
Nothing
λ: let q = Perm.mk [3,1,2,4,6,5,7]
λ: Perm.isSeparable q                             -- q does avoid 2413 and 3142, and hence is separable
True
λ: import Data.Maybe
λ: fromJust . SeparatingTree.mk $ Perm.toPoints q -- so that a separating tree of q can be obtained
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

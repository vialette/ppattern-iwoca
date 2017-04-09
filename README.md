# ppattern

```haskell
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: let p = Perm.mk [3,1,4,2,6,5,7]
λ: p
[3,1,4,2,6,5,7]
λ: Perm.reversal p
[7,5,6,2,4,1,3]
λ: Perm.complement p
[5,7,4,6,2,3,1]
λ: Perm.reversalComplement p
[1,3,2,6,4,7,5]
λ: Perm.inverse p
[2,4,1,3,6,5,7]
λ:
```

## Separable permutations

```haskell
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: import qualified Data.Algorithm.PPattern.SeparatingTree as SeparatingTree
λ: let p = Perm.mk [3,1,4,2,6,5,7]
λ: Perm.isSeparable p
False
λ: SeparatingTree.mk $ Perm.toPoints p
Nothing
λ: let q = Perm.mk [3,1,2,4,6,5,7]
λ: Perm.isSeparable q
True
λ: SeparatingTree.mk $ Perm.toPoints q
Just "+ Interval (1,7)"
 "+ Interval (1,6)"
  "+ Interval (1,4)"
   "- Interval (1,3)"
    Point (1,3)
    "+ Interval (1,2)"
     Point (2,1)
     Point (3,2)
   Point (4,4)
  "- Interval (5,6)"
   Point (5,6)
   Point (6,5)
 Point (7,7)

λ:
```

# ppattern

```haskell
λ: import qualified Data.Algorithm.PPattern.Perm as Perm
λ: let p = Perm.mk [3, 1, 4, 2, 6, 5, 7]
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

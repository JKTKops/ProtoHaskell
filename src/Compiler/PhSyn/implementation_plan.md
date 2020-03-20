In keeping with the idea of a depth-first approach, we will ignore a large part of the PhExpr
datatype on this pass through the compiler.

Imagine it is defined thus:

```hs
data PhExpr id
     = PhVar id
     | PhLit PhLit
     | PhLam (MatchGroup id)
     | PhApp (LPhExpr id) (LPhExpr id)
     | PhPar (LPhExpr id)
     | PhCase (LPhExpr id) (MatchGroup id)
     | PhLet (LPhLocalBinds id) (LPhExpr id)
     | Typed (LPhType id) (LPhExpr id)
```

This subset of the type is the "kernel" and should be relatively easy to translate into Core.
The goal of the _desugarer_ is to translate the rest of the PhExpr type into the kernel. For
example, NegApp e becomes PhApp (PhVar (PHC.Num.negate)) e.

By starting with only the kernel, we can skip the desugarer entirely on this pass through the
compiler and write it later.

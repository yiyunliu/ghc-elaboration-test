# ghc-elaboration-test
Use the ghc api to elaborate expressions.

The ghc wiki page is out-of-date and doesn't cover how to convert a string into an elaborated core expression.

```haskell
elaborateExpr :: GhcMonad m => String -> m CoreExpr
```

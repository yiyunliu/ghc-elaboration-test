# ghc-elaboration-test
Use the ghc api to elaborate expressions.

The ghc wiki page is out-of-date and doesn't cover how to convert a string into an elaborated core expression.

## Minimal example for the plugin-mode symbol resolution issue
To build the plugin, run:
```haskell
stack build
```
The plugin tries to typecheck the expression `testPlus 100` and output the checked core expression with debug information.

[QQQ.hs](QQQ.hs) contains the definition of `testPlus`. [PPP.hs](PPP.hs) imports [QQQ.hs](QQQ.hs).

After running `stack ghc -- -fplugin=Test.Plugin  PPP.hs`, you should be able to see the following output:
```
[1 of 2] Compiling QQQ              ( QQQ.hs, QQQ.o ) [Impure plugin forced reco
(main:QQQ.testPlus{v r4F} [lid] :: ghc-prim:GHC.Types.Int{(w) tc 3u}
                                   -> ghc-prim:GHC.Types.Int{(w) tc 3u})
  ((ghc-prim:GHC.Types.I#{(w) v 6j} [gid[DataCon]] :: ghc-prim:GHC.Prim.Int#{(w)
                                                      -> ghc-prim:GHC.Types.Int{
     100#)
[2 of 2] Compiling PPP              ( PPP.hs, PPP.o )
(main:QQQ.testPlus{v r4F} [gid] :: ghc-prim:GHC.Types.Int{(w) tc 3u}
                                   -> ghc-prim:GHC.Types.Int{(w) tc 3u})
  ((ghc-prim:GHC.Types.I#{(w) v 6j} [gid[DataCon]] :: ghc-prim:GHC.Prim.Int#{(w)
                                                      -> ghc-prim:GHC.Types.Int{
     100#)
```
Note that `testPlus` is annotated with `[lid]` when compiling [QQQ.hs](QQQ.hs), whereas `testPlus` is annotated with `[gid]` in [PPP.hs](PPP.hs). `testPlus` is an exported symbol in both cases, but when elaborating the expression in the file where `testPlus` is defined, the symbol is labeled as local instead of global.

There are [a few functions](https://hackage.haskell.org/package/ghc-8.10.2/docs/src/Var.html#globaliseId) that changes the flag of the Id. I think `deSugar` calls some of these functions (but not `dsLExpr`, which is used in `elabRnExpr`). Even if that is the case, `deSugar` isn't directly usable since it seems to operate on an entire module rather than a single expression.

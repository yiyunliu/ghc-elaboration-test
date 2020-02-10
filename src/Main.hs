{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Main where

import           GHC
import           TcEvidence
import           DsBinds
import           OccName
import           DsExpr
import           FastString
import           HsPat
import           SrcLoc
import           DsMonad
import           Control.Monad
import           CoreSyn
import           Exception
import           Inst
import           Panic
import           Desugar
import           TcRnMonad
import           TcHsSyn
import           RnExpr
import           GhcMonad
import           TcSimplify
import           PrelNames
import           Outputable
import           GHC.LanguageExtensions.Type
import           HscTypes
import           ErrUtils
import           HscMain
import           TcExpr
import           HsExpr
import           RdrName
import           BasicTypes

import           GHC.Paths                      ( libdir )
--GHC.Paths is available via cabal install ghc-paths

import           DynFlags
targetFile :: String
targetFile = "c.hs"

main :: IO ()
main = do
  str <- runGhc (Just libdir) $ do
    res    <- example
    dflags <- getSessionDynFlags
    return $ showSDoc dflags $ ppr res
  putStrLn str

example2 :: (GhcMonad m) => m CoreProgram
example2 = do
  dflags <- getSessionDynFlags
  let dflags' = foldl xopt_set dflags [Cpp, ImplicitPrelude, MagicHash]
  void $ setSessionDynFlags
    (          dflags' { hscTarget = HscInterpreted
                       , ghcLink   = LinkInMemory
                       , ghcMode   = CompManager
                       } -- `gopt_set` Opt_ImplicitImportQualified
             -- `gopt_set` Opt_PIC
             -- `gopt_set` Opt_DeferTypedHoles
    `gopt_set` Opt_DeferOutOfScopeVariables
             -- `xopt_set` MagicHash
             -- `xopt_set` DeriveGeneric
             -- `xopt_set` StandaloneDeriving
    )
  target <- guessTarget targetFile Nothing
  setTargets [target]
  void $ load LoadAllTargets
  let modBName = mkModuleName "QQQ"
  modSum   <- getModSummary modBName
  p        <- parseModule modSum
  t        <- typecheckModule p
  mod_guts <- coreModule <$> desugarModule t
  pure $ mg_binds mod_guts




example :: (GhcMonad m) => m _
example = do
  dflags <- getSessionDynFlags
  let dflags' = foldl xopt_set dflags [Cpp, ImplicitPrelude, MagicHash]
  void
    $          setSessionDynFlags
    $          dflags' { hscTarget = HscInterpreted
                       , ghcLink   = LinkInMemory
                       , ghcMode   = CompManager
                       }
    `gopt_set` Opt_DeferTypedHoles
  target <- guessTarget targetFile Nothing
  setTargets [target]
  void $ load LoadAllTargets
  let modBName = mkModuleName "QQQ"
  modSum <- getModSummary modBName
  setContext
    [ IIModule $ ms_mod_name modSum
    , IIDecl (simpleImportDecl (moduleName gHC_NUM))
    ]
  setSessionDynFlags
    $          dflags' { hscTarget = HscInterpreted
                       , ghcLink   = LinkInMemory
                       , ghcMode   = CompManager
                       }
    `gopt_set` Opt_DeferTypedHoles
  void $ execStmt "let {infixr 1 ==>; True ==> False = False; _ ==> _ = True}"
                  execOptions
  -- e0 <- elaborateExpr
  --   TM_Inst
  --   "(\\ x y ->  ymappend x y) :: YSem a => jfioewa -> a -> a"
  e1       <- elaborateExpr TM_Inst "(\\ x y ->  ymappend x y)"
  e1'      <- elaborateExpr TM_Inst "((\\ x y ->  x + y) ) :: Int -> Int -> Int"
  -- e1'' <- elaborateExpr
  --   TM_Inst

  -- names <- getNamesInScope
  bindings <- getBindings
  -- e2 <- elaborateExpr TM_Inst "(\\ x y z ->  x ==> y ==> z)"
  -- e3 <- elaborateExpr TM_Inst "(1 + (1 :: Num a => a)) :: Int"
  -- e4 <- exprType TM_Inst "1 GHC.Num.+ 1"
  -- -- "\x -> x"
  -- let hse = (noLoc
  --             (HsLam
  --               NoExt
  --               (MG
  --                 NoExt
  --                 (noLoc
  --                   [ (noLoc
  --                       (Match
  --                         NoExt
  --                         LambdaExpr
  --                         [ noLoc
  --                             (VarPat
  --                               NoExt
  --                               (noLoc (mkUnqual varName (mkFastString "1invalid21$lq##x")))
  --                             )
  --                         ]
  --                         (GRHSs NoExt
  --                          [noLoc (GRHS NoExt [] (noLoc (HsApp NoExt
  --                                                        (noLoc (HsVar NoExt (noLoc (mkQual varName (mkFastString "DS", mkFastString "<>")))))
  --                                                        (noLoc (HsVar NoExt (noLoc (mkUnqual varName (mkFastString "1invalid21$lq##x"))))))))]
  --                          (noLoc (EmptyLocalBinds NoExt)))
  --                       )
  --                     )
  --                   ]
  --                 )
  --                 Generated
  --               )
  --             )
  --           )
  -- e6 <- elaborateHsExpr
  --   TM_Default
  --   hse
  -- -- return (e0, e1, e2, e3, e4, e6)
  return (e1, e1', bindings)



elaborateExpr :: GhcMonad m => TcRnExprMode -> String -> m (Maybe CoreExpr)
elaborateExpr mode expr =
  withSession $ \hsc_env -> liftIO $ hscElabExpr hsc_env mode expr

elaborateHsExpr
  :: GhcMonad m => TcRnExprMode -> LHsExpr GhcPs -> m (Maybe CoreExpr)
elaborateHsExpr mode expr =
  withSession $ \hsc_env -> liftIO $ hscElabHsExpr hsc_env mode expr

hscElabHsExpr :: HscEnv -> TcRnExprMode -> LHsExpr GhcPs -> IO (Maybe CoreExpr)
hscElabHsExpr hsc_env0 mode expr = runInteractiveHsc hsc_env0 $ do
  hsc_env <- getHscEnv
  liftIO $ snd <$> elabRnExpr hsc_env mode expr


hscElabExpr :: HscEnv -> TcRnExprMode -> String -> IO (Maybe CoreExpr)
hscElabExpr hsc_env0 mode expr = runInteractiveHsc hsc_env0 $ do
  hsc_env     <- getHscEnv
  parsed_expr <- hscParseExpr expr
  liftIO $ snd <$> elabRnExpr hsc_env mode parsed_expr

elabRnExpr
  :: HscEnv -> TcRnExprMode -> LHsExpr GhcPs -> IO (Messages, Maybe CoreExpr)
elabRnExpr hsc_env mode rdr_expr = do
  runTcInteractive hsc_env $ do
    (rn_expr, _fvs) <- rnLExpr rdr_expr
    failIfErrsM
    uniq <- newUnique
    let fresh_it = itName uniq (getLoc rdr_expr)
        orig     = lexprCtOrigin rn_expr
    (tclvl, lie, (tc_expr, res_ty)) <- pushLevelAndCaptureConstraints $ do
      (_tc_expr, expr_ty) <- tcInferSigma rn_expr
      expr_ty'            <- if inst
        then snd <$> deeplyInstantiate orig expr_ty
        else return expr_ty
      return (_tc_expr, expr_ty')
    (_, _, evbs, residual, _) <- simplifyInfer tclvl
                                               infer_mode
                                               []    {- No sig vars -}
                                               [(fresh_it, res_ty)]
                                               lie
    evbs'    <- perhaps_disable_default_warnings $ simplifyInteractive residual
    ztc_expr <- zonkTopLExpr
      (mkHsDictLet (EvBinds evbs') (mkHsDictLet evbs tc_expr))
    initDsTc $ do
      dsLExpr ztc_expr


 where
  (inst, infer_mode, perhaps_disable_default_warnings) = case mode of
    TM_Inst    -> (True, NoRestrictions, id)
    TM_NoInst  -> (False, NoRestrictions, id)
    TM_Default -> (True, EagerDefaulting, unsetWOptM Opt_WarnTypeDefaults)





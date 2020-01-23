module Main where

import           GHC
import           CoreSyn
import           Inst
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

import           GHC.Paths                      ( libdir )
--GHC.Paths is available via cabal install ghc-paths

import           DynFlags
targetFile :: String
targetFile = "B.hs"

main :: IO ()
main = do
  res <- example
  str <- runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    return $ showSDoc dflags $ ppr res
  putStrLn str

example :: IO CoreExpr
example =
  defaultErrorHandler defaultFatalMessager defaultFlushOut
    $ runGhc (Just libdir)
    $ do
        dflags <- getSessionDynFlags
        let dflags' = foldl xopt_set dflags [Cpp, ImplicitPrelude, MagicHash]
        setSessionDynFlags dflags' { hscTarget = HscInterpreted
                                   , ghcLink   = LinkInMemory
                                   , ghcMode   = CompManager
                                   }
        target <- guessTarget targetFile Nothing
        setTargets [target]
        load LoadAllTargets
        let modBName = mkModuleName "B"
        modSum <- getModSummary modBName
        setContext [IIModule $ ms_mod_name modSum]
        elaborateExpr TM_Inst
                      "(\\ x y ->  ymappend x y == ymappend y x) :: (Eq a, YSem a) => a -> a -> Bool"

elaborateExpr :: GhcMonad m => TcRnExprMode -> String -> m CoreExpr
elaborateExpr mode expr =
  withSession $ \hsc_env -> liftIO $ hscElabExpr hsc_env mode expr

hscElabExpr :: HscEnv -> TcRnExprMode -> String -> IO CoreExpr
hscElabExpr hsc_env0 mode expr = runInteractiveHsc hsc_env0 $ do
  hsc_env     <- getHscEnv
  parsed_expr <- hscParseExpr expr
  ioMsgMaybe $ elabRnExpr hsc_env mode parsed_expr

elabRnExpr
  :: HscEnv -> TcRnExprMode -> LHsExpr GhcPs -> IO (Messages, Maybe CoreExpr)
elabRnExpr hsc_env mode rdr_expr = do
  (msgs, maybe_tc_expr) <- runTcInteractive hsc_env $ do
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
    (_, _, _, residual, _) <- simplifyInfer tclvl
                                            infer_mode
                                            []    {- No sig vars -}
                                            [(fresh_it, res_ty)]
                                            lie
    _ <- perhaps_disable_default_warnings $ simplifyInteractive residual
    zonkTopLExpr tc_expr
  case maybe_tc_expr of
    Nothing      -> pure (msgs, Nothing)
    Just tc_expr -> deSugarExpr hsc_env tc_expr
 where
  (inst, infer_mode, perhaps_disable_default_warnings) = case mode of
    TM_Inst    -> (True, NoRestrictions, id)
    TM_NoInst  -> (False, NoRestrictions, id)
    TM_Default -> (True, EagerDefaulting, unsetWOptM Opt_WarnTypeDefaults)



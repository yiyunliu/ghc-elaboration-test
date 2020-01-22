module Main where

import GHC
import CoreSyn
import Inst
import Desugar
import TcRnMonad
import TcHsSyn
import RnExpr
import GhcMonad
import TcSimplify
import PrelNames
import Outputable
import GHC.LanguageExtensions.Type
import HscTypes
import ErrUtils
import HscMain
import TcExpr
 
import GHC.Paths ( libdir )
--GHC.Paths is available via cabal install ghc-paths

import DynFlags
targetFile = "B.hs"

main :: IO ()
main = do
   res <- example
   str <- runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      return $ showSDoc dflags $ ppr res
   putStrLn str
 
example = 
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = foldl xopt_set dflags [Cpp, ImplicitPrelude, MagicHash]
        setSessionDynFlags dflags'
          {hscTarget = HscInterpreted, ghcLink = LinkInMemory , ghcMode = CompManager }
        target <- guessTarget targetFile Nothing
        setTargets [target]
        load LoadAllTargets
        let modBName = mkModuleName "B"
        modSum <- getModSummary modBName
        -- p <- parseModule modSum
        -- t <- typecheckModule p
        -- d <- desugarModule t
        -- l <- loadModule d
        setContext [IIModule $ ms_mod_name modSum]
        myE <- elaborateExpr  "(\\ x y ->  Sum x <> Sum y) :: Num a => a -> a -> Sum a "
        -- n <- getNamesInScope
        -- c <- return $ coreModule d
 
        -- g <- getModuleGraph
        -- mapM showModule (mgModSummaries g)

        return $ myE

elaborateExpr :: GhcMonad m => String -> m CoreExpr
elaborateExpr expr = withSession $ \hsc_env -> do
  liftIO $ hscElabExpr hsc_env expr

hscElabExpr :: HscEnv -> String -> IO CoreExpr
hscElabExpr hsc_env0 expr = runInteractiveHsc hsc_env0 $ do
  hsc_env <- getHscEnv
  parsed_expr <- hscParseExpr expr
  ioMsgMaybe $ elabRnExpr hsc_env parsed_expr

elabRnExpr :: HscEnv
           -> LHsExpr GhcPs
           -> IO (Messages, Maybe CoreExpr)
elabRnExpr hsc_env rdr_expr
  = do
     (msgs, maybe_tc_expr) <- (runTcInteractive hsc_env $
                               do {
       
                                 (rn_expr, _fvs) <- rnLExpr rdr_expr ;
                                 failIfErrsM ;
       
                                 uniq <- newUnique ;
                                 let { fresh_it  = itName uniq (getLoc rdr_expr)
                                     ; orig = lexprCtOrigin rn_expr } ;
                                 pp <- pushLevelAndCaptureConstraints $
                                   do {(_tc_expr, expr_ty) <- tcInferSigma rn_expr;
                                       expr_ty' <- snd <$> deeplyInstantiate orig expr_ty;
                                       pure (_tc_expr, expr_ty')} ;

                                 (tclvl, lie, (tc_expr, res_ty)) <- pure pp;
                                 (qtvs, dicts, _, residual, _)
                                     <- simplifyInfer tclvl NoRestrictions
                                                      []    {- No sig vars -}
                                                      [(fresh_it, res_ty)]
                                                      lie ;

                                 -- Ignore the dictionary bindings
                                 _ <- simplifyInteractive residual ;

                                 zonkTopLExpr tc_expr
                                 })
     case maybe_tc_expr of
       Nothing -> pure (msgs, Nothing)
       Just tc_expr -> deSugarExpr hsc_env tc_expr


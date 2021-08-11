{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Test.Plugin where

import           GHC
import           Control.Monad.State
import           TcType
import           Class
import           TcEvidence
import           GhcPlugins              hiding ( getHscEnv )
import           DsBinds
import qualified Data.Map.Strict               as OM
import           OccName
import           DsExpr
import           FastString
-- import           HsPat
import           ClsInst
import           SrcLoc
import           DsMonad
import           Control.Monad
import           CoreSyn
import           CoreFVs
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
-- import           HsExpr
import           RdrName
import           BasicTypes
import           GHC.Paths                      ( libdir )
import           TcOrigin
--GHC.Paths is available via cabal install ghc-paths

import           DynFlags
targetFile :: String
-- targetFile = "c.hs"
targetFile = "functor.hs"




plugin :: Plugin
plugin = defaultPlugin {
  typeCheckResultAction = testHook
  }
  where testHook _ _ gblEnv = do
          e  <- elabRnExpr TM_Inst hse
          df <- getDynFlags
          liftIO $ putStrLn $ showSDocDebug df $ ppr e
          pure gblEnv
        hse = mkHsApp (nlHsVar (mkVarUnqual "testPlus")) (nlHsIntLit 100)




elabRnExpr
  :: TcRnExprMode -> LHsExpr GhcPs -> TcRn CoreExpr
elabRnExpr mode rdr_expr = do
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
    evbs' <- perhaps_disable_default_warnings $ simplifyInteractive residual
    full_expr <- zonkTopLExpr (mkHsDictLet (EvBinds evbs') (mkHsDictLet evbs tc_expr))
    initDsTc $ dsLExpr full_expr
 where
  (inst, infer_mode, perhaps_disable_default_warnings) = case mode of
    TM_Inst    -> (True, NoRestrictions, id)
    TM_NoInst  -> (False, NoRestrictions, id)
    TM_Default -> (True, EagerDefaulting, unsetWOptM Opt_WarnTypeDefaults)

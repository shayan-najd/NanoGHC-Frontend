{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Printer ()  where

import U.Outputable ( Outputable(..)
                    , OutputableBndr(..)
                    , pprPrefixVar
                    , pprInfixVar
                    , ftext
                    , SDoc
                    , text
                    , empty
                    , (<+>)
                    , (<>)
                    , fcat
                    , integer
                    , comma
                    , pprWithCommas
                    , nest
                    , sep
                    , hsep
                    , space
                    , parens
                    , pprDeeper
                    , dcolon
                    , hang
                    , ptext)

import AST
import Prelude hiding ((<>))
import U.FastString

import GHC.Lexeme (isVarSymChar,startsVarSym,startsConSym)

instance Outputable ExpPS where
    ppr e | isAtomicExp e || isQuietExp e = ppr_expr e
          | otherwise                     = pprDeeper (ppr_expr e)
      where
        isAtomicExp :: ExpPS -> Bool
        isAtomicExp (Var {})   = True
        isAtomicExp (Lit {})   = True
        isAtomicExp (Par _ e') = isAtomicExp e'
        isAtomicExp _          = False

        isQuietExp :: ExpPS -> Bool
        isQuietExp (Par {}) = True
        isQuietExp (App {}) = True
        isQuietExp _        = False

ppr_expr :: ExpPS -> SDoc
ppr_expr (Var _ x)        = pprPrefixOcc x
ppr_expr (Lit _ _ lit)    = integer lit
ppr_expr (Typ _ expr sig) = hang (nest 2 (ppr_expr expr) <+> dcolon) 4 (ppr sig)
ppr_expr (Abs _ p e)      = text "\\" <+> ppr p <+> text "->" <+> pprDeeper (ppr e)
ppr_expr e@(App {})
  = ppr_apps e []
  where
    ppr_apps :: ExpPS -> [ExpPS] -> SDoc
    ppr_apps (App _ fun arg) args = ppr_apps fun (arg : args)
    ppr_apps fun             args = hang (ppr_expr fun) 2
                                     (sep (map pprParendExp args))
    pprParendExp :: ExpPS -> SDoc
    pprParendExp e'@(Lit {}) = ppr e'
    pprParendExp e'@(Var {}) = ppr e'
    pprParendExp e'@(Tup {}) = ppr e'
    pprParendExp e'@(Par {}) = ppr e'
    pprParendExp e'          = parens (ppr e')
ppr_expr (Tup _ expr1 expr2)
  = parens (fcat (ppr_tup_args [expr1 , expr2]))
  where
    ppr_tup_args []       = []
    ppr_tup_args (e : es) = (ppr_expr e <> punc es) : ppr_tup_args es

    punc (_ : _) = comma <> space
    punc []      = empty
ppr_expr (Let _ d expr@(Let{}))
                          = sep [ hang (text "let") 2
                                 (hsep [pprDeeper (ppr d), ptext (sLit "in")])
                                , ppr_expr expr]
ppr_expr (Let _ d expr)   = sep [ hang (text "let") 2 (pprDeeper (ppr d))
                                , hang (text "in")  2 (ppr expr)]
ppr_expr (Par _ e)        = parens (ppr_expr e)
ppr_expr (NewExp _)       = error "Impossible!"

instance Outputable TypPS where
    ppr ty = ppr_mono_ty False (prepare ty)

prepare :: TypPS -> TypPS
prepare (ParTyp _ ty) = prepare  ty
prepare ty            = ty

ppr_mono_ty :: Bool -> TypPS -> SDoc
ppr_mono_ty _    (VarTyp _ x)          = pprPrefixOcc x
ppr_mono_ty prec (FunTyp _ ty1 ty2)    = (if prec then parens else id)
                                         (sep [ ppr_mono_ty True ty1
                                            , text "->" <+>
                                              ppr_mono_ty False ty2])
ppr_mono_ty _    (TupTyp _ ty1 ty2)    = parens (pprWithCommas ppr [ty1,ty2])
ppr_mono_ty _    (ParTyp _ ty)         = parens (ppr_mono_ty False ty)
ppr_mono_ty _    (NewTyp _)            = error "Impossible!"

instance Outputable PatPS where
    ppr (VarPat _ x)   = pprPrefixOcc x
    ppr (TupPat _ x y) = parens (pprWithCommas pprPrefixOcc [x,y])
    ppr (NewPat _)  = error "Impossible!"

instance Outputable DecPS where
    ppr (Dec _ p e) = sep [ppr p, nest 2 (text "=" <+> pprDeeper (ppr e))]
    ppr (NewDec _)  = error "Impossible!"

isLexSym :: FastString -> Bool
isLexSym cs = isLexConSym cs || isLexVarSym cs

isLexConSym :: FastString -> Bool
isLexConSym cs
  | nullFS cs          = False
  | cs == (fsLit "->") = True
  | otherwise          = startsConSym (headFS cs)

isLexVarSym :: FastString -> Bool
isLexVarSym fs = case (if nullFS fs then [] else unpackFS fs) of
  []     -> False
  (c:cs) -> startsVarSym c && all isVarSymChar cs

instance Outputable Nam where
    ppr (Nam n) = ftext n


instance OutputableBndr Nam where
    pprBndr _ n             = ppr n

    pprInfixOcc  rdr@(Nam n) = pprInfixVar  (isLexSym n) (ppr rdr)
    pprPrefixOcc rdr@(Nam n) = pprPrefixVar (isLexSym n) (ppr rdr)

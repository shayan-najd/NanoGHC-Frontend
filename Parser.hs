{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
module Parser
  ( parseExpression
  , parseType
  , parsePattern
  , parseDeclaration
  ) where

import AST
import U.SrcLoc
import U.Outputable
import Printer ()
import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn7 :: (ExpPS) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (ExpPS)
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (ExpPS) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (ExpPS)
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: (ExpPS) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> (ExpPS)
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: (ExpPS) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> (ExpPS)
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: (TypPS) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> (TypPS)
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (TypPS) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (TypPS)
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (PatPS) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> (PatPS)
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: (DecPS) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> (DecPS)
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyInTok :: ((Located Token)) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> ((Located Token))
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xff\xff\xfd\xff\x02\x00\x02\x00\xff\xff\x3f\x00\x02\x00\x00\x00\x02\x00\x3e\x00\xff\xff\x00\x00\x00\x00\x00\x00\x40\x00\x3d\x00\x3d\x00\x3d\x00\x3c\x00\xfd\xff\x00\x00\x33\x00\x3a\x00\xfd\xff\x37\x00\xfd\xff\xff\xff\x27\x00\x38\x00\x3b\x00\x00\x00\xff\xff\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\xfd\xff\x00\x00\x39\x00\x2d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x28\x00\x36\x00\x29\x00\x0c\x00\x2f\x00\x00\x00\x07\x00\x00\x00\x0a\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x00\x00\x00\x00\x00\x00\x32\x00\x00\x00\x30\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1c\x00\x18\x00\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf7\xff\xf5\xff\x00\x00\x00\x00\x00\x00\xf4\xff\xf3\xff\xeb\xff\x00\x00\x00\x00\x00\x00\x00\x00\xef\xff\x00\x00\xee\xff\x00\x00\xfa\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf6\xff\x00\x00\x00\x00\xf1\xff\x00\x00\xea\xff\xf0\xff\xec\xff\x00\x00\xfb\xff\x00\x00\x00\x00\xf9\xff\xf8\xff\xf2\xff\xed\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x05\x00\x04\x00\x05\x00\x04\x00\x05\x00\x05\x00\x0b\x00\x0a\x00\x03\x00\x0c\x00\x0a\x00\x03\x00\x0c\x00\x03\x00\x06\x00\x07\x00\x06\x00\x07\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x03\x00\x06\x00\x07\x00\x06\x00\x01\x00\x02\x00\x03\x00\x06\x00\x04\x00\x05\x00\x04\x00\x05\x00\x04\x00\x05\x00\x04\x00\x05\x00\x01\x00\x06\x00\x07\x00\x06\x00\x0d\x00\x09\x00\x08\x00\x03\x00\xff\xff\x09\x00\xff\xff\x08\x00\x0a\x00\xff\xff\x0d\x00\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x09\x00\x14\x00\x0a\x00\x0b\x00\x28\x00\x12\x00\x0b\x00\x15\x00\x0c\x00\x1e\x00\x0d\x00\x0c\x00\x0d\x00\x0d\x00\x0d\x00\x0e\x00\x1d\x00\x0e\x00\x0f\x00\x29\x00\x16\x00\x06\x00\x07\x00\x2a\x00\x16\x00\x06\x00\x07\x00\x2b\x00\x16\x00\x06\x00\x07\x00\x23\x00\x16\x00\x06\x00\x07\x00\x1b\x00\x16\x00\x06\x00\x07\x00\x15\x00\x16\x00\x06\x00\x07\x00\x0d\x00\x22\x00\x23\x00\x10\x00\x05\x00\x06\x00\x07\x00\x2d\x00\x24\x00\x12\x00\x27\x00\x12\x00\x18\x00\x12\x00\x11\x00\x12\x00\x20\x00\x26\x00\x27\x00\x2e\x00\xff\xff\x21\x00\x18\x00\x1b\x00\x00\x00\x1a\x00\x00\x00\x18\x00\x1d\x00\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (4, 21) [
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21)
	]

happy_n_terms = 14 :: Int
happy_n_nonterms = 8 :: Int

happyReduce_4 = happyMonadReduce 3# 0# happyReduction_4
happyReduction_4 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut11 happy_x_3 of { happy_var_3 -> 
	( ams (Typ (sLL happy_var_1 happy_var_3) happy_var_1 happy_var_3)
                                    [mu AnnDcolon happy_var_2])}}}
	) (\r -> happyReturn (happyIn7 r))

happyReduce_5 = happySpecReduce_1  0# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn7
		 (happy_var_1
	)}

happyReduce_6 = happyMonadReduce 4# 1# happyReduction_6
happyReduction_6 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	( ams (Abs (sLL happy_var_1 happy_var_4) (Nam (getVARID happy_var_2)) happy_var_4)
                                    [mu AnnLam happy_var_1,mu AnnRarrow happy_var_3])}}}}
	) (\r -> happyReturn (happyIn8 r))

happyReduce_7 = happyMonadReduce 4# 1# happyReduction_7
happyReduction_7 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut14 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	( ams (Let (sLL happy_var_1 happy_var_4) happy_var_2 happy_var_4)
                                    [mj AnnLet happy_var_1,mj AnnIn happy_var_3])}}}}
	) (\r -> happyReturn (happyIn8 r))

happyReduce_8 = happySpecReduce_1  1# happyReduction_8
happyReduction_8 happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (happy_var_1
	)}

happyReduce_9 = happySpecReduce_2  2# happyReduction_9
happyReduction_9 happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_2 of { happy_var_2 -> 
	happyIn9
		 (App (sLL happy_var_1 happy_var_2) happy_var_1 happy_var_2
	)}}

happyReduce_10 = happySpecReduce_1  2# happyReduction_10
happyReduction_10 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (happy_var_1
	)}

happyReduce_11 = happySpecReduce_1  3# happyReduction_11
happyReduction_11 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (Var (getSpan happy_var_1) (Nam (getVARID happy_var_1))
	)}

happyReduce_12 = happySpecReduce_1  3# happyReduction_12
happyReduction_12 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (Lit (getSpan happy_var_1) (getINTEGERs happy_var_1)
                                                   (getINTEGER happy_var_1)
	)}

happyReduce_13 = happyMonadReduce 5# 3# happyReduction_13
happyReduction_13 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	case happyOutTok happy_x_5 of { happy_var_5 -> 
	( ams (Tup (sLL happy_var_1 happy_var_5) happy_var_2 happy_var_4)
                                    [mj AnnOpenP happy_var_1,mj AnnComma happy_var_3,
                                     mj AnnCloseP happy_var_5])}}}}}
	) (\r -> happyReturn (happyIn10 r))

happyReduce_14 = happyMonadReduce 3# 3# happyReduction_14
happyReduction_14 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	( ams (Par (sLL happy_var_1 happy_var_3) happy_var_2)
                                    [mj AnnOpenP happy_var_1,mj AnnCloseP happy_var_3])}}}
	) (\r -> happyReturn (happyIn10 r))

happyReduce_15 = happyMonadReduce 3# 4# happyReduction_15
happyReduction_15 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut11 happy_x_3 of { happy_var_3 -> 
	( ams (FunTyp (sLL happy_var_1 happy_var_3) happy_var_1 happy_var_3)
                                    [mu AnnRarrow happy_var_2])}}}
	) (\r -> happyReturn (happyIn11 r))

happyReduce_16 = happySpecReduce_1  4# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn11
		 (happy_var_1
	)}

happyReduce_17 = happySpecReduce_1  5# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn12
		 (VarTyp (getSpan happy_var_1) (Nam (getCONID happy_var_1))
	)}

happyReduce_18 = happyMonadReduce 5# 5# happyReduction_18
happyReduction_18 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	case happyOut11 happy_x_4 of { happy_var_4 -> 
	case happyOutTok happy_x_5 of { happy_var_5 -> 
	( ams (TupTyp (sLL happy_var_1 happy_var_5) happy_var_2 happy_var_4)
                                    [mj AnnOpenP happy_var_1,mj AnnComma happy_var_3,
                                     mj AnnCloseP happy_var_5])}}}}}
	) (\r -> happyReturn (happyIn12 r))

happyReduce_19 = happyMonadReduce 3# 5# happyReduction_19
happyReduction_19 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	( ams (ParTyp (sLL happy_var_1 happy_var_3) happy_var_2)
                                    [mj AnnOpenP happy_var_1,mj AnnCloseP happy_var_3])}}}
	) (\r -> happyReturn (happyIn12 r))

happyReduce_20 = happyMonadReduce 1# 6# happyReduction_20
happyReduction_20 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut10 happy_x_1 of { happy_var_1 -> 
	( checkPattern happy_var_1)}
	) (\r -> happyReturn (happyIn13 r))

happyReduce_21 = happyMonadReduce 3# 7# happyReduction_21
happyReduction_21 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut7 happy_x_3 of { happy_var_3 -> 
	( ams (Dec (sLL happy_var_1 happy_var_3) happy_var_1 happy_var_3)
                                    [mj AnnEqual happy_var_2])}}}
	) (\r -> happyReturn (happyIn14 r))

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	L _ ITeof -> happyDoAction 13# tk action sts stk;
	L _ ITin -> cont 1#;
	L _ ITlet -> cont 2#;
	L _ ITequal -> cont 3#;
	L _ (ITlam _) -> cont 4#;
	L _ IToparen -> cont 5#;
	L _ ITcparen -> cont 6#;
	L _ ITcomma -> cont 7#;
	L _ (ITdcolon _) -> cont 8#;
	L _ (ITrarrow _) -> cont 9#;
	L _ (ITvarid    _) -> cont 10#;
	L _ (ITconid    _) -> cont 11#;
	L _ (ITinteger _ _) -> cont 12#;
	_ -> happyError' tk
	})

happyError_ 13# tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (>>=)
happyReturn :: () => a -> P a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => ((Located Token)) -> P a
happyError' tk = (\token -> happyError) tk

parseExpression = happySomeParser where
  happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (happyOut7 x))

parseType = happySomeParser where
  happySomeParser = happyThen (happyParse 1#) (\x -> happyReturn (happyOut11 x))

parsePattern = happySomeParser where
  happySomeParser = happyThen (happyParse 2#) (\x -> happyReturn (happyOut13 x))

parseDeclaration = happySomeParser where
  happySomeParser = happyThen (happyParse 3#) (\x -> happyReturn (happyOut14 x))

happySeq = happyDoSeq


happyError :: P a
happyError = srcParseFail

getVARID        (L _ (ITvarid       x)) = x
getCONID        (L _ (ITconid       x)) = x
getINTEGER      (L _ (ITinteger _   x)) = x
getINTEGERs     (L _ (ITinteger src _)) = src

sLL :: (Spanned a , Spanned b) =>
       a -> b -> SrcSpan
sLL a b = a `seq` b `seq`
          combineSrcSpans (getSpan a) (getSpan b)

mj :: AnnKeywordId -> Located Token -> AddAnn
mj a l s = addAnnotation s a (getLoc l)

mu :: AnnKeywordId -> Located Token -> AddAnn
mu a l s = addAnnotation s (toUnicodeAnn a l) (getLoc l)
 where
  isUnicode :: Located Token -> Bool
  isUnicode (L _ (ITdcolon         iu)) = iu == UnicodeSyntax
  isUnicode (L _ (ITrarrow         iu)) = iu == UnicodeSyntax
  isUnicode _                           = False

  toUnicodeAnn :: AnnKeywordId -> Located Token -> AnnKeywordId
  toUnicodeAnn a t = if isUnicode t
                     then unicodeAnn a
                     else a

ams :: Spanned a => a -> [AddAnn] -> P a
ams a bs = addAnnsAt (getSpan a) bs >> return a

checkPattern :: ExpPS -> P PatPS
checkPattern e0 = do
 case e0 of
   Var sp x                   -> return (VarPat sp x)
   Tup sp (Var _ x) (Var _ y) -> return (TupPat sp x y)
   _                          -> failSpanMsgP (getSpan e0)
                                   (text "Parse error in pattern:" <+> ppr e0)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 11 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "/opt/ghc/7.10.1/lib/ghc-7.10.1/include/ghcversion.h" #-}

















{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}





-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 46 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st


indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = indexShortOffAddr happyGotoOffsets st1
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i



          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

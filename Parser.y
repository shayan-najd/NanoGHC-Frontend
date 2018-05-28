{
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

}

%expect 1 -- shift/reduce conflicts

%token
 'in'           { L _ ITin }
 'let'          { L _ ITlet }
 '='            { L _ ITequal }
 '\\'           { L _ (ITlam _)}
 '('            { L _ IToparen }
 ')'            { L _ ITcparen }
 ','            { L _ ITcomma }
 '::'           { L _ (ITdcolon _) }
 '->'           { L _ (ITrarrow _) }
 VARID          { L _ (ITvarid    _) }
 CONID          { L _ (ITconid    _) }
 INTEGER        { L _ (ITinteger _ _) }

%monad     { P } { >>= } { return }
%lexer     { lexer } { L _ ITeof }
%tokentype { (Located Token) }

%name parseExpression  exp
%name parseType        typ
%name parsePattern     pat
%name parseDeclaration dec
%%

exp      :: { ExpPS }
          : exp1 '::' typ       {% ams (Typ (sLL $1 $>) $1 $3)
                                    [mu AnnDcolon $2] }
          | exp1                { $1 }

exp1     :: { ExpPS }
          : '\\' VARID '->' exp {% ams (Abs (sLL $1 $>) (Nam (getVARID $2)) $4)
                                    [mu AnnLam $1,mu AnnRarrow $3] }
          | 'let' dec 'in' exp  {% ams (Let (sLL $1 $>) $2 $4)
                                    [mj AnnLet $1,mj AnnIn $3] }
          | exp2                { $1 }

exp2     :: { ExpPS }
          : exp2 exp3           { App (sLL $1 $>) $1 $2 }
          | exp3                { $1 }

exp3    :: { ExpPS }
         : VARID                { Var (getSpan $1) (Nam (getVARID $1)) }
         | INTEGER              { Lit (getSpan $1) (getINTEGERs $1)
                                                   (getINTEGER $1)}
         | '(' exp ',' exp ')'  {% ams (Tup (sLL $1 $>) $2 $4)
                                    [mj AnnOpenP $1,mj AnnComma $3,
                                     mj AnnCloseP $5] }
         | '(' exp ')'          {% ams (Par (sLL $1 $>) $2)
                                    [mj AnnOpenP $1,mj AnnCloseP $3] }

typ     :: { TypPS }
         : typ1 '->' typ        {% ams (FunTyp (sLL $1 $>) $1 $3)
                                    [mu AnnRarrow $2] }
         | typ1                 { $1 }


typ1    :: { TypPS }
         : CONID                { VarTyp (getSpan $1) (Nam (getCONID $1)) }
         | '(' typ ',' typ ')'  {% ams (TupTyp (sLL $1 $>) $2 $4)
                                    [mj AnnOpenP $1,mj AnnComma $3,
                                     mj AnnCloseP $5] }
         | '(' typ ')'          {% ams (ParTyp (sLL $1 $>) $2)
                                    [mj AnnOpenP $1,mj AnnCloseP $3] }

pat     :: { PatPS }
pat      : exp3                 {% checkPattern $1 }

dec     :: { DecPS }
         : pat '=' exp          {% ams (Dec (sLL $1 $>) $1 $3)
                                    [mj AnnEqual $2] }

{
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


}

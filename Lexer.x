{
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Lexer
       ( mkPState
       , lexer
       , P(..)
       , Token(..)
       , getPState
       , ParseResult(..)
       , AddAnn
       , addAnnsAt
       , addAnnotation
       , getSrcLoc
       , srcParseFail
       , failSpanMsgP
       , AnnKeywordId(..)
       , AnnotationComment(..)
       , IsUnicodeSyntax(..)
       , ApiAnnKey
       , unicodeAnn) where

import Control.Monad
import Control.Monad.Fail
import Data.Char
import Data.Maybe
import Data.Word
import Data.Data

import U.SrcLoc
import U.FastString
import U.Outputable
import U.StringBuffer

}

$unispace       = \x05
$nl             = [\n\r\f]
$whitechar      = [$nl\v\ $unispace]
$white_no_nl    = $whitechar # \n
$tab            = \t
$ascdigit       = 0-9
$unidigit       = \x03
$decdigit       = $ascdigit
$digit          = [$ascdigit $unidigit]
$special        = [\(\)\,\;\[\]\`\{\}]
$ascsymbol      = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]
$unisymbol      = \x04
$symbol         = [$ascsymbol $unisymbol] # [$special \_\"\']
$unilarge       = \x01
$asclarge       = [A-Z]
$large          = [$asclarge $unilarge]
$unismall       = \x02
$ascsmall       = [a-z]
$small          = [$ascsmall $unismall \_]
$unigraphic     = \x06
$graphic        = [$small $large $symbol $digit $special $unigraphic \"\']
$uniidchar      = \x07
$idchar         = [$small $large $digit $uniidchar \']
$pragmachar     = [$small $large $digit]
$docsym         = [\| \^ \* \$]

@varid          = $small $idchar*
@conid          = $large $idchar*
@decimal        = $decdigit+

haskell :-

$white_no_nl+                           ;
$tab                                    { warnTab }

"{-"                                    { nested_comment lexToken }
"-- " .*                                { lineCommentToken }
"---"\-* .*                             { lineCommentToken }

<bol> {
  \n                                    ;
  ()                                    { do_bol }
}
<0> {
  \:\:                                  { special (ITdcolon NormalSyntax)}
  \n                                    { begin bol }
  \(                                    { special IToparen }
  \)                                    { special ITcparen }
  \,                                    { special ITcomma }
  \=                                    { special ITequal }
  \\                                    { special (ITlam NormalSyntax) }
  \-\>                                  { special (ITrarrow NormalSyntax) }
  \.                                    { special ITdot }
  ∷                                     { special (ITdcolon UnicodeSyntax) }
  →                                     { special (ITrarrow UnicodeSyntax) }
  λ                                     { special (ITlam    UnicodeSyntax) }
  let                                   { special ITlet }
  in                                    { special ITin }
  @varid                                { varid }
  @conid                                { idtoken conid }
  @decimal                              { tok_num positive 0 0 decimal }
  \- @decimal                           { tok_num negative 1 1 decimal }
}


{
data Token
  = ITlet
  | ITin
  | ITdcolon       IsUnicodeSyntax
  | ITequal
  | ITlam          IsUnicodeSyntax
  | ITrarrow       IsUnicodeSyntax
  | ITdarrow       IsUnicodeSyntax
  | ITdot
  | ITocurly
  | ITccurly
  | ITvocurly
  | ITvccurly
  | IToparen
  | ITcparen
  | ITsemi
  | ITcomma
  | ITvarid        FastString
  | ITconid        FastString
  | ITinteger      String Integer
  | ITeof
  | ITlineComment  String
  | ITblockComment String
  deriving Show

instance Outputable Token where
  ppr x = text (show x)


type Action = RealSrcSpan -> StringBuffer -> Int -> P (RealLocated Token)

special :: Token -> Action
special tok span _buf _len = return (L span tok)

idtoken :: (StringBuffer -> Int -> Token) -> Action
idtoken f span buf len = return (L span $! (f buf len))

strtoken :: (String -> Token) -> Action
strtoken f span buf len =
  return (L span $! (f $! lexemeToString buf len))

begin :: Int -> Action
begin code _span _str _len = do pushLexState code; lexToken

lineCommentToken :: Action
lineCommentToken = strtoken ITlineComment

nested_comment :: P (RealLocated Token) -> Action
nested_comment cont span buf len = do
  input <- getInput
  go (reverse $ lexemeToString buf len) (1::Int) input
  where
    go commentAcc 0 input = do
      setInput input
      docCommentEnd input commentAcc ITblockComment buf span

    go commentAcc n input = case alexGetChar' input of
      Nothing -> errBrace input span
      Just ('-',input) -> case alexGetChar' input of
        Nothing  -> errBrace input span
        Just ('\125',input) -> go ('\125':'-':commentAcc) (n-1) input -- '}'
        Just (_,_)          -> go ('-':commentAcc) n input
      Just ('\123',input) -> case alexGetChar' input of  -- '{' char
        Nothing  -> errBrace input span
        Just ('-',input) -> go ('-':'\123':commentAcc) (n+1) input
        Just (_,_)       -> go ('\123':commentAcc) n input
      Just (c,input) -> go (c:commentAcc) n input

errBrace :: AlexInput -> RealSrcSpan -> P a
errBrace (AI end _) span = failLocMsgP (realSrcSpanStart span) end "unterminated `{-'"

varid :: Action
varid span buf len = return (L span (ITvarid fs))
  where
    !fs = lexemeToFastString buf len

conid :: StringBuffer -> Int -> Token
conid buf len = ITconid $! lexemeToFastString buf len

-- Variations on the integral numeric literal.
tok_integral :: (String -> Integer -> Token)
             -> (Integer -> Integer)
             -> Int -> Int
             -> (Integer, (Char -> Int))
             -> Action
tok_integral itint transint transbuf translen (radix,char_to_int) span buf len
 = return $ L span $ itint (lexemeToString buf len)
       $! transint $ parseUnsignedInteger
       (offsetBytes transbuf buf) (subtract translen len) radix char_to_int

-- some conveniences for use with tok_integral
tok_num :: (Integer -> Integer)
        -> Int -> Int
        -> (Integer, (Char->Int)) -> Action
tok_num = tok_integral ITinteger

positive :: (Integer -> Integer)
positive = id

negative :: (Integer -> Integer)
negative = negate

decimal :: (Integer, Char -> Int)
decimal = (10, \ c -> ord c - ord '0')

do_bol :: Action
do_bol span _str _len =
  do _ <- popLexState
     lexToken

warnTab :: Action
warnTab srcspan _buf _len =
  do addTabWarning srcspan
     lexToken

data ParseResult a
  = POk PState a
  | PFailed SrcSpan SDoc


data PState = PState {
        buffer     :: StringBuffer,
        tab_first  :: Maybe RealSrcSpan,
        tab_count  :: !Int,
        last_tk    :: Maybe Token,
        last_loc   :: RealSrcSpan, -- pos of previous token
        last_len   :: !Int,        -- len of previous token
        loc        :: RealSrcLoc,  -- current loc (end of prev token + 1)
        lex_state  :: [Int],
        annotations :: [(ApiAnnKey,[SrcSpan])],
        comment_q :: [Located AnnotationComment],
        annotations_comments :: [(SrcSpan,[Located AnnotationComment])]
     }

newtype P a = P { unP :: PState -> ParseResult a }

instance Functor P where
  fmap = liftM

instance Applicative P where
  pure a = a `seq` (P $ \s -> POk s a)
  (<*>)  = ap

instance Monad P where
  (P m) >>= k = P $ \ s ->
        case m s of
                POk s1 a         -> (unP (k a)) s1
                PFailed span err -> PFailed span err
  fail  = failP

instance MonadFail P where
  fail = failP

failP :: String -> P a
failP msg = P $ \s -> PFailed (RealSrcSpan (last_loc s)) (text msg)

failLocMsgP :: RealSrcLoc -> RealSrcLoc -> String -> P a
failLocMsgP loc1 loc2 str = P $ \_ -> PFailed (RealSrcSpan (mkRealSrcSpan loc1 loc2)) (text str)

failSpanMsgP :: SrcSpan -> SDoc -> P a
failSpanMsgP span msg = P $ \_ -> PFailed span msg


getSrcLoc :: P RealSrcLoc
getSrcLoc = P $ \s@(PState{ loc=loc }) -> POk s loc

setLastToken :: RealSrcSpan -> Int -> P ()
setLastToken loc len = P $ \s -> POk s {
  last_loc=loc,
  last_len=len
  } ()

setLastTk :: Token -> P ()
setLastTk tk = P $ \s -> POk s { last_tk = Just tk } ()

data AlexInput = AI RealSrcLoc StringBuffer

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AI loc s)
  | atEnd s   = Nothing
  | otherwise = byte `seq` loc' `seq` s' `seq`
                Just (byte, (AI loc' s'))
  where (c,s') = nextChar s
        loc'   = advanceSrcLoc loc c
        byte   = fromIntegral $ ord adj_c
        non_graphic     = '\x00'
        upper           = '\x01'
        lower           = '\x02'
        digit           = '\x03'
        symbol          = '\x04'
        space           = '\x05'
        other_graphic   = '\x06'
        uniidchar       = '\x07'
        adj_c
          | c <= '\x07' = non_graphic
          | c <= '\x7f' = c
          | otherwise =
                case generalCategory c of
                  UppercaseLetter       -> upper
                  LowercaseLetter       -> lower
                  TitlecaseLetter       -> upper
                  ModifierLetter        -> uniidchar -- see #10196
                  OtherLetter           -> lower -- see #1103
                  NonSpacingMark        -> uniidchar -- see #7650
                  SpacingCombiningMark  -> other_graphic
                  EnclosingMark         -> other_graphic
                  DecimalNumber         -> digit
                  LetterNumber          -> other_graphic
                  OtherNumber           -> digit -- see #4373
                  ConnectorPunctuation  -> symbol
                  DashPunctuation       -> symbol
                  OpenPunctuation       -> other_graphic
                  ClosePunctuation      -> other_graphic
                  InitialQuote          -> other_graphic
                  FinalQuote            -> other_graphic
                  OtherPunctuation      -> symbol
                  MathSymbol            -> symbol
                  CurrencySymbol        -> symbol
                  ModifierSymbol        -> symbol
                  OtherSymbol           -> symbol
                  Space                 -> space
                  _other                -> non_graphic

alexGetChar' :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar' (AI loc s)
  | atEnd s   = Nothing
  | otherwise = c `seq` loc' `seq` s' `seq`
                --trace (show (ord c)) $
                Just (c, (AI loc' s'))
  where (c,s') = nextChar s
        loc'   = advanceSrcLoc loc c

getInput :: P AlexInput
getInput = P $ \s@PState{ loc=l, buffer=b } -> POk s (AI l b)

setInput :: AlexInput -> P ()
setInput (AI l b) = P $ \s -> POk s{ loc=l, buffer=b } ()

pushLexState :: Int -> P ()
pushLexState ls = P $ \s@PState{ lex_state=l } -> POk s{lex_state=ls:l} ()

popLexState :: P Int
popLexState = P $ \s@PState{ lex_state=ls:l } -> POk s{ lex_state=l } ls

getLexState :: P Int
getLexState = P $ \s@PState{ lex_state=ls:_ } -> POk s ls

addTabWarning :: RealSrcSpan -> P ()
addTabWarning srcspan
 = P $ \s@PState{tab_first=tf, tab_count=tc} ->
       let tf' = if isJust tf then tf else Just srcspan
           tc' = tc + 1
           s'  = s{tab_first = tf', tab_count = tc'}
       in POk s' ()

srcParseErr :: StringBuffer -> Int -> SDoc
srcParseErr buf len
  = if null token
         then text "parse error (possibly incorrect indentation or mismatched brackets)"
         else text "parse error on input" <+> quotes (text token)
  where token = lexemeToString (offsetBytes (-len) buf) len

srcParseFail :: P a
srcParseFail = P $ \PState{ buffer = buf, last_len = len,
                            last_loc = last_loc } ->
    PFailed (RealSrcSpan last_loc) (srcParseErr buf len)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AI _ buf) = prevChar buf '\n'

lexer :: (Located Token -> P a) -> P a
lexer cont = do
  (L span tok) <- lexToken
  case tok of
    ITeof -> addAnnotationOnly noSrcSpan AnnEofPos (RealSrcSpan span)
    _ -> return ()

  if (isComment tok)
    then queueComment (L (RealSrcSpan span) tok) >> lexer cont
    else cont (L (RealSrcSpan span) tok)


lexToken :: P (RealLocated Token)
lexToken = do
  inp@(AI loc1 buf) <- getInput
  sc <- getLexState
  case alexScanUser 0 inp sc of
    AlexEOF -> do
        let span = mkRealSrcSpan loc1 loc1
        setLastToken span 0
        return (L span ITeof)
    AlexError (AI loc2 buf) ->
        reportLexError loc1 loc2 buf "lexical error"
    AlexSkip inp2 _ -> do
        setInput inp2
        lexToken
    AlexToken inp2@(AI end buf2) _ t -> do
        setInput inp2
        let span = mkRealSrcSpan loc1 end
        let bytes = byteDiff buf buf2
        span `seq` setLastToken span bytes
        lt <- t span buf bytes
        case unLoc lt of
          ITlineComment _  -> return lt
          ITblockComment _ -> return lt
          lt' -> do
            setLastTk lt'
            return lt

reportLexError :: RealSrcLoc -> RealSrcLoc -> StringBuffer -> [Char] -> P a
reportLexError loc1 loc2 buf str
  | atEnd buf = failLocMsgP loc1 loc2 (str ++ " at end of input")
  | otherwise =
  let c = fst (nextChar buf)
  in if c == '\0' -- decoding errors are mapped to '\0', see utf8DecodeChar#
     then failLocMsgP loc2 loc2 (str ++ " (UTF-8 decoding error)")
     else failLocMsgP loc1 loc2 (str ++ " at character " ++ show c)


-- | Encapsulated call to addAnnotation, requiring only the SrcSpan of
--   the AST construct the annotation belongs to; together with the
--   AnnKeywordId, this is is the key of the annotation map
type AddAnn = SrcSpan -> P ()

addAnnotation :: SrcSpan          -- SrcSpan of enclosing AST construct
              -> AnnKeywordId     -- The first two parameters are the key
              -> SrcSpan          -- The location of the keyword itself
              -> P ()
addAnnotation l a v = do
  addAnnotationOnly l a v
  allocateComments l

addAnnotationOnly :: SrcSpan -> AnnKeywordId -> SrcSpan -> P ()
addAnnotationOnly l a v = P $ \s -> POk s {
  annotations = ((l,a), [v]) : annotations s
  } ()

-- |Given a location and a list of AddAnn, apply them all to the location.
addAnnsAt :: SrcSpan -> [AddAnn] -> P ()
addAnnsAt loc anns = mapM_ (\a -> a loc) anns

queueComment :: Located Token -> P()
queueComment c = P $ \s -> POk s {
  comment_q = commentToAnnotation c : comment_q s
  } ()

allocateComments :: SrcSpan -> P ()
allocateComments ss = P $ \s ->
  let
    (before,rest)  = break (\(L l _) -> isSubspanOf l ss) (comment_q s)
    (middle,after) = break (\(L l _) -> not (isSubspanOf l ss)) rest
    comment_q' = before ++ after
    newAnns = if null middle then []
                             else [(ss,middle)]
  in
    POk s {
       comment_q = comment_q'
     , annotations_comments = newAnns ++ (annotations_comments s)
     } ()

commentToAnnotation :: Located Token -> Located AnnotationComment
commentToAnnotation (L l (ITlineComment s))     = L l (AnnLineComment s)
commentToAnnotation (L l (ITblockComment s))    = L l (AnnBlockComment s)
commentToAnnotation _                           = error "commentToAnnotation"

isComment :: Token -> Bool
isComment (ITlineComment     _)   = True
isComment (ITblockComment    _)   = True
isComment _ = False


docCommentEnd :: AlexInput -> String -> (String -> Token) -> StringBuffer ->
                 RealSrcSpan -> P (RealLocated Token)
docCommentEnd input commentAcc docType buf span = do
  setInput input
  let (AI loc nextBuf) = input
      comment = reverse commentAcc
      span' = mkRealSrcSpan (realSrcSpanStart span) loc
      last_len = byteDiff buf nextBuf

  span `seq` setLastToken span' last_len
  return (L span' (docType comment))


getPState :: P PState
getPState = P $ \s -> POk s s

-- | Creates a parse state from a 'DynFlags' value
mkPState :: StringBuffer -> RealSrcLoc -> PState
mkPState = mkPStatePure

mkPStatePure :: StringBuffer -> RealSrcLoc -> PState
mkPStatePure buf loc =
  PState {
      buffer        = buf,
      tab_first     = Nothing,
      tab_count     = 0,
      last_tk       = Nothing,
      last_loc      = mkRealSrcSpan loc loc,
      last_len      = 0,
      loc           = loc,
      lex_state     = [bol, 0],
      annotations = [],
      comment_q = [],
      annotations_comments = []
    }

type ApiAnnKey = (SrcSpan,AnnKeywordId)

data AnnKeywordId
    = AnnLet
    | AnnIn
    | AnnDcolon -- ^ '::'
    | AnnDcolonU -- ^ '::', unicode variant
    | AnnEqual
    | AnnLam
    | AnnLamU
    | AnnRarrow -- ^ '->'
    | AnnRarrowU -- ^ '->', unicode variant
    | AnnDot    -- ^ '.'
    | AnnOpenC   -- ^ '{'
    | AnnCloseC -- ^ '}'
    | AnnOpenP   -- ^ '('
    | AnnCloseP -- ^ ')'
    | AnnSemi -- ^ ';'
    | AnnComma -- ^ as a list separator
    | AnnName
    | AnnVal  -- ^ e.g. INTEGER
    | AnnEofPos
    | AnnOpen   -- ^ '(\#' or '{-\# LANGUAGE' etc
    | AnnClose -- ^  '\#)' or '\#-}'  etc
    | AnnCommaTuple -- ^ in a RdrName for a tuple
    deriving (Eq, Ord, Data, Show)

data AnnotationComment
  = AnnLineComment     String
  | AnnBlockComment    String
 deriving (Eq, Ord, Data, Show)

data IsUnicodeSyntax
  = UnicodeSyntax
  | NormalSyntax
  deriving (Eq, Ord, Data, Show)

unicodeAnn :: AnnKeywordId -> AnnKeywordId
unicodeAnn AnnLam        = AnnLamU
unicodeAnn AnnDcolon     = AnnDcolonU
unicodeAnn AnnRarrow     = AnnRarrowU
unicodeAnn ann           = ann

}

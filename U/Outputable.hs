{-# OPTIONS_GHC -Wall #-}
module U.Outputable
       ( showSDocUnsafe
       , showSDoc
       , vcat
       , ppWhen
       , ($$)
       , quotes
       , Outputable(..)
       , OutputableBndr(..)
       , SDoc
       , text
       , ftext
       , empty
       , braces
       , pprPrefixVar
       , pprInfixVar
       , (<+>)
       , (<>)
       , fcat
       , integer
       , comma
       , fsep
       , brackets
       , pprWithCommas
       , nest
       , sep
       , hsep
       , space
       , parens
       , pprDeeper
       , dcolon
       , hang
       , ptext) where

import Prelude hiding ((<>))
import U.FastString
import qualified U.Pretty
import U.Pretty           ( Doc, Mode(..) )

import Data.Word

data Depth = AllTheWay
           | PartWay Int

defaultUserStyle  :: Depth
defaultUserStyle = AllTheWay

newtype SDoc = SDoc { runSDoc :: Depth -> Doc }

docToSDoc :: Doc -> SDoc
docToSDoc d = SDoc (const d)

pprDeeper :: SDoc -> SDoc
pprDeeper d = SDoc $ \ctx -> case ctx of
  (PartWay 0) -> U.Pretty.text "..."
  (PartWay n) -> runSDoc d (PartWay (n-1))
  _           -> runSDoc d ctx

showSDoc :: Int -> SDoc -> String
showSDoc llength sdoc = renderWithStyle llength sdoc defaultUserStyle

renderWithStyle :: Int -> SDoc -> Depth -> String
renderWithStyle llength sdoc sty
  = let s = U.Pretty.style{ U.Pretty.mode = PageMode,
                          U.Pretty.lineLength = llength }
    in U.Pretty.renderStyle s $ runSDoc sdoc sty


empty    :: SDoc
empty           = docToSDoc $ U.Pretty.empty

char     :: Char       -> SDoc
char c          = docToSDoc $ U.Pretty.char c

text     :: String     -> SDoc
text s          = docToSDoc $ U.Pretty.text s

ftext    :: FastString -> SDoc
ftext s         = docToSDoc $ U.Pretty.ftext s

ptext    :: LitString  -> SDoc
ptext s         = docToSDoc $ U.Pretty.ptext s

int      :: Int        -> SDoc
int n           = docToSDoc $ U.Pretty.int n

integer  :: Integer    -> SDoc
integer n       = docToSDoc $ U.Pretty.integer n

parens :: SDoc -> SDoc
parens d        = SDoc $ U.Pretty.parens . runSDoc d

braces :: SDoc -> SDoc
braces d        = SDoc $ U.Pretty.braces . runSDoc d

brackets :: SDoc -> SDoc
brackets d      = SDoc $ U.Pretty.brackets . runSDoc d

quotes :: SDoc -> SDoc
quotes d = char '‘' <> d <> char '’'

dcolon :: SDoc
dcolon     = docToSDoc $ U.Pretty.char '∷'

comma :: SDoc
comma      = docToSDoc $ U.Pretty.comma

space :: SDoc
space      = docToSDoc $ U.Pretty.space

nest :: Int -> SDoc -> SDoc
nest n d    = SDoc $ U.Pretty.nest n . runSDoc d

(<>) :: SDoc -> SDoc -> SDoc
(<>) d1 d2  = SDoc $ \sty -> (U.Pretty.<>)  (runSDoc d1 sty) (runSDoc d2 sty)

(<+>) :: SDoc -> SDoc -> SDoc
(<+>) d1 d2 = SDoc $ \sty -> (U.Pretty.<+>) (runSDoc d1 sty) (runSDoc d2 sty)

($$) :: SDoc -> SDoc -> SDoc
($$) d1 d2  = SDoc $ \sty -> (U.Pretty.$$)  (runSDoc d1 sty) (runSDoc d2 sty)

hsep :: [SDoc] -> SDoc
hsep ds = SDoc $ \sty -> U.Pretty.hsep [runSDoc d sty | d <- ds]

vcat :: [SDoc] -> SDoc
vcat ds = SDoc $ \sty -> U.Pretty.vcat [runSDoc d sty | d <- ds]

sep :: [SDoc] -> SDoc
sep ds  = SDoc $ \sty -> U.Pretty.sep  [runSDoc d sty | d <- ds]

fsep :: [SDoc] -> SDoc
fsep ds = SDoc $ \sty -> U.Pretty.fsep [runSDoc d sty | d <- ds]

fcat :: [SDoc] -> SDoc
fcat ds = SDoc $ \sty -> U.Pretty.fcat [runSDoc d sty | d <- ds]

hang :: SDoc -> Int -> SDoc -> SDoc
hang d1 n d2   = SDoc $ \sty -> U.Pretty.hang (runSDoc d1 sty) n (runSDoc d2 sty)

punctuate :: SDoc -> [SDoc] -> [SDoc]
punctuate _ []     = []
punctuate p (d:ds) = go d ds
                   where
                     go d' [] = [d']
                     go d' (e:es) = (d' <> p) : go e es

ppWhen :: Bool -> SDoc -> SDoc
ppWhen True  doc = doc
ppWhen False _   = empty

class Outputable a where
        ppr :: a -> SDoc
        pprPrec :: Rational -> a -> SDoc

        ppr = pprPrec 0
        pprPrec _ = ppr

instance Outputable Char where
    ppr c = text [c]

instance Outputable Bool where
    ppr True  = text "True"
    ppr False = text "False"

instance Outputable Ordering where
    ppr LT = text "LT"
    ppr EQ = text "EQ"
    ppr GT = text "GT"

instance Outputable Int where
    ppr n = int n

instance Outputable Word16 where
    ppr n = integer $ fromIntegral n

instance Outputable Word32 where
    ppr n = integer $ fromIntegral n

instance Outputable Word where
    ppr n = integer $ fromIntegral n

instance Outputable () where
    ppr _ = text "()"

instance (Outputable a) => Outputable [a] where
    ppr xs = brackets (fsep (punctuate comma (map ppr xs)))

instance (Outputable a, Outputable b) => Outputable (a, b) where
    ppr (x,y) = parens (sep [ppr x <> comma, ppr y])

instance Outputable a => Outputable (Maybe a) where
    ppr Nothing  = text "Nothing"
    ppr (Just x) = text "Just" <+> ppr x

instance (Outputable a, Outputable b) => Outputable (Either a b) where
    ppr (Left x)  = text "Left"  <+> ppr x
    ppr (Right y) = text "Right" <+> ppr y

-- may not be used
instance (Outputable a, Outputable b, Outputable c) => Outputable (a, b, c) where
    ppr (x,y,z) =
      parens (sep [ppr x <> comma,
                   ppr y <> comma,
                   ppr z ])

instance (Outputable a, Outputable b, Outputable c, Outputable d) =>
         Outputable (a, b, c, d) where
    ppr (a,b,c,d) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d])

instance (Outputable a, Outputable b, Outputable c, Outputable d, Outputable e) =>
         Outputable (a, b, c, d, e) where
    ppr (a,b,c,d,e) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d <> comma,
                   ppr e])

instance (Outputable a, Outputable b, Outputable c, Outputable d, Outputable e, Outputable f) =>
         Outputable (a, b, c, d, e, f) where
    ppr (a,b,c,d,e,f) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d <> comma,
                   ppr e <> comma,
                   ppr f])

instance (Outputable a, Outputable b, Outputable c, Outputable d, Outputable e, Outputable f, Outputable g) =>
         Outputable (a, b, c, d, e, f, g) where
    ppr (a,b,c,d,e,f,g) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d <> comma,
                   ppr e <> comma,
                   ppr f <> comma,
                   ppr g])

instance Outputable FastString where
    ppr fs = ftext fs           -- Prints an unadorned string,
                                -- no double quotes or anything



data BindingSite

-- | When we print a binder, we often want to print its type too.
-- The @OutputableBndr@ class encapsulates this idea.
class Outputable a => OutputableBndr a where
   pprBndr :: BindingSite -> a -> SDoc
   pprBndr _b x = ppr x

   pprPrefixOcc, pprInfixOcc :: a -> SDoc

pprPrefixVar :: Bool -> SDoc -> SDoc
pprPrefixVar is_operator pp_v
  | is_operator = parens pp_v
  | otherwise   = pp_v

-- Put a name in backquotes if it's not an operator
pprInfixVar :: Bool -> SDoc -> SDoc
pprInfixVar is_operator pp_v
  | is_operator = pp_v
  | otherwise   = char '`' <> pp_v <> char '`'

pprWithCommas :: (a -> SDoc) -- ^ The pretty printing function to use
              -> [a]         -- ^ The things to be pretty printed
              -> SDoc        -- ^ 'SDoc' where the things have been pretty printed,
                             -- comma-separated and finally packed into a paragraph.
pprWithCommas pp xs = fsep (punctuate comma (map pp xs))

showSDocUnsafe :: SDoc -> String
showSDocUnsafe = showSDoc 100

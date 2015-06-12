{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

module Text.Namelist.Pretty
    ( DString, toString
    , Pretty(..)
    , PrettyConfig(..), Mode(..)
    ) where

import Data.Complex(Complex((:+)))
import Data.CaseInsensitive (CI, original)
import Data.List(intersperse)
import Data.Char(toUpper)

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid(Monoid(..))
#endif

import Data.Monoid((<>))
import Data.Default.Class(Default(def))

import Text.Namelist.Types

newtype DString = DString (String -> String)

instance Monoid DString where
    mempty = DString id
    mappend (DString a) (DString b) = DString (a . b)
    {-# INLINABLE mempty #-}
    {-# INLINABLE mappend #-}

toString :: DString -> String
toString (DString d) = d []

fromString :: String -> DString
fromString s = DString (s ++)

fromShow :: Show a => a -> DString
fromShow = fromString . show

singleton :: Char -> DString
singleton c = DString (c:)

data Mode
    = Compact
    | Large { indent :: Int }

data PrettyConfig = PrettyConfig
    { prettyLogical :: String -> String
    , pairSeparator :: String
    , mode          :: Mode
    }

cfgCompact :: PrettyConfig -> Bool
cfgCompact (PrettyConfig _ _ Compact) = True
cfgCompact _ = False

instance Default PrettyConfig where
    def = PrettyConfig (map toUpper) " = " (Large 2)
    {-# INLINABLE def #-}

class Pretty a where
    ppr :: PrettyConfig -> a -> DString

instance Pretty Index where
    ppr _ (Index i)     = fromShow i
    ppr _ (Range l h s) = opt l <> colon <> opt h <> step
      where
        opt   = maybe mempty fromShow
        colon = singleton ':'
        step  = maybe mempty (\t -> colon <> fromShow t) s
    {-# INLINABLE ppr #-}

surround :: Char -> Char -> DString -> DString
surround a b c = singleton a <> c <> singleton b

instance Pretty [Index] where
    ppr _ = surround '(' ')' . mconcat . intersperse (singleton ',') . map (ppr def)
    {-# INLINABLE ppr #-}

ci :: CI String -> DString
ci = fromString . original

instance Pretty Key where
    ppr _ (Key k)       = ci k
    ppr _ (Indexed k i) = ci k <> ppr def i
    ppr _ (Sub k s)     = ci k <> singleton '%' <> ppr def s
    {-# INLINABLE ppr #-}

instance Pretty Value where
    ppr _   (Integer i) = fromShow i
    ppr _   (Real r)    = fromShow r

    ppr cfg (Complex (r :+ i)) = singleton '(' <> fromShow r <> singleton ',' <> sp <> fromShow i <> singleton ')'
      where
        sp = if cfgCompact cfg then mempty else singleton ' '

    ppr cfg (Logical True)  = fromString . prettyLogical cfg $ if cfgCompact cfg then "T" else ".True."
    ppr cfg (Logical False) = fromString . prettyLogical cfg $ if cfgCompact cfg then "F" else ".False."

    ppr _   (String s)
        | '\'' `notElem` s = singleton '\'' <> fromString s <> singleton '\''
        | '"'  `notElem` s = singleton '"'  <> fromString s <> singleton '"'
        | otherwise = singleton '\'' <> fromString (concatMap escape s) <> singleton '\''
      where
        escape '\'' = "''"
        escape a    = [a]

    ppr cfg (Array a) = mconcat . intersperse sep $ map (ppr cfg) a
      where
        sep = if cfgCompact cfg then singleton ',' else fromString ", "

    ppr cfg (r :* v)  = fromShow r <> singleton '*' <> ppr cfg v

    ppr _   Null = mempty
    {-# INLINABLE ppr #-}

instance Pretty Pair where
    ppr cfg (k := v) = ppr cfg k <> equal <> ppr cfg v
      where
        equal = if cfgCompact cfg then singleton '=' else fromString " = "
    {-# INLINABLE ppr #-}

instance Pretty [Pair] where
    ppr cfg@PrettyConfig{ mode = Compact } =
        mconcat . intersperse (fromString ", ") . map (ppr cfg)

    ppr cfg@PrettyConfig{ mode = Large i } =
        mconcat . intersperse (fromString ",\n") . map (\p -> ind <> ppr cfg p)
      where
        ind = fromString $ replicate i ' '
    {-# INLINABLE ppr #-}

instance Pretty Group where
    ppr cfg (Group g ps) = singleton '&' <> ci g <> gsp <> ppr cfg ps <> ged
      where
        gsp = if cfgCompact cfg then singleton ' ' else singleton '\n'
        ged = if cfgCompact cfg
              then fromString " /"
              else if null ps then singleton '/' else fromString "\n/"
    {-# INLINABLE ppr #-}

instance Pretty [Group] where
    ppr cfg@PrettyConfig{ mode = Compact } =
        mconcat . intersperse (singleton ' ') . map (ppr cfg)

    ppr cfg@PrettyConfig{ mode = Large _ } =
        mconcat . intersperse (singleton '\n') . map (ppr cfg)
    {-# INLINABLE ppr #-}

module Text.Namelist.Pretty
    ( DString, toString
    , prettyNamelist
    , PrettyConfig(..), Mode(..)
    ) where

import Data.Complex
import Data.CaseInsensitive (CI, original)
import Data.List(intersperse)
import Data.Char(toUpper)

import Data.Monoid((<>))
import Data.Default.Class

import Text.Namelist.Types

newtype DString = DString (String -> String)

instance Monoid DString where
    mempty = DString id
    mappend (DString a) (DString b) = DString (a . b)

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

prettyIndex :: Index -> DString
prettyIndex (Index i)     = fromShow i
prettyIndex (Range a b)   = fromShow a <> singleton ':' <> fromShow b
prettyIndex (Step  a b s) = prettyIndex (Range a b) <> singleton ':' <> fromShow s

prettyIndices :: [Index] -> DString
prettyIndices = mconcat . intersperse (singleton ',') . map prettyIndex

ci :: CI String -> DString
ci = fromString . original

prettyKey :: Key -> DString
prettyKey (Key k)       = ci k
prettyKey (Indexed k i) = ci k <> singleton '(' <> prettyIndices i <> singleton ')'
prettyKey (Sub k s)     = ci k <> singleton '%' <> prettyKey s

prettyValue :: PrettyConfig -> Value -> DString
prettyValue _   (Integer i) = fromShow i
prettyValue _   (Real r)    = fromShow r

prettyValue cfg (Complex (r :+ i)) = singleton '(' <> fromShow r <> singleton ',' <> sp <> fromShow i <> singleton ')'
  where
    sp = if cfgCompact cfg then mempty else singleton ' '

prettyValue cfg (Logical True)  = fromString . prettyLogical cfg $ if cfgCompact cfg then "T" else ".True."
prettyValue cfg (Logical False) = fromString . prettyLogical cfg $ if cfgCompact cfg then "F" else ".False."

prettyValue _   (String s)
    | '\'' `notElem` s = singleton '\'' <> fromString s <> singleton '\''
    | '"'  `notElem` s = singleton '"'  <> fromString s <> singleton '"'
    | otherwise = singleton '\'' <> fromString (concatMap escape s) <> singleton '\''
  where
    escape '\'' = "''"
    escape a    = [a]

prettyValue cfg (Array a) = mconcat . intersperse sep $ map (prettyValue cfg) a
  where
    sep = if cfgCompact cfg then singleton ',' else fromString ", "

prettyValue cfg (r :* v)  = fromShow r <> singleton '*' <> prettyValue cfg v

prettyValue _   Null = mempty

prettyPair :: PrettyConfig -> Pair -> DString
prettyPair cfg (k := v) = prettyKey k <> equal <> prettyValue cfg v
  where
    equal = if cfgCompact cfg then singleton '=' else fromString " = "

prettyPairs :: PrettyConfig -> [Pair] -> DString
prettyPairs cfg@PrettyConfig{ mode = Compact } =
    mconcat . intersperse (fromString ", ") . map (prettyPair cfg)

prettyPairs cfg@PrettyConfig{ mode = Large i } =
    mconcat . intersperse (fromString ",\n") . map (\p -> ind <> prettyPair cfg p)
  where
    ind = fromString $ replicate i ' '

prettyGroup :: PrettyConfig -> Group -> DString
prettyGroup cfg (Group g ps) = singleton '&' <> ci g <> gsp <> prettyPairs cfg ps <> ged
  where
    gsp = if cfgCompact cfg then singleton ' ' else singleton '\n'
    ged = if cfgCompact cfg
          then fromString " /"
          else if null ps then singleton '/' else fromString "\n/"

prettyNamelist :: PrettyConfig -> [Group] -> DString
prettyNamelist cfg@PrettyConfig{ mode = Compact } =
    mconcat . intersperse (singleton ' ') . map (prettyGroup cfg)

prettyNamelist cfg@PrettyConfig{ mode = Large _ } =
    mconcat . intersperse (singleton '\n') . map (prettyGroup cfg)

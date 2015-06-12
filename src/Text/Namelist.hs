module Text.Namelist
    ( module Text.Namelist.Types
    , parse
    , Pretty, pretty
    , prettyCompact
    ) where

import Text.Namelist.Types
import Text.Namelist.Parser
import Text.Namelist.Pretty
import Data.Default.Class

import qualified Text.Parsec as P

parse :: String -> Either P.ParseError [Group]
parse = P.parse namelist ""

prettyWith :: Pretty a => PrettyConfig -> a -> String
prettyWith cfg = toString . ppr cfg

prettyCompact :: Pretty a => a -> String
prettyCompact = prettyWith def { mode = Compact, pairSeparator = "=" }

pretty :: Pretty a => a -> String
pretty = prettyWith def

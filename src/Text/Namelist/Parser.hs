{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Text.Namelist.Parser (namelist) where

import Text.Parsec hiding (letter)
import Data.Complex(Complex((:+)))
import Data.Char (toUpper, toLower, isDigit)
import Data.CaseInsensitive (CI, mk)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<$), (<*>), (<*), (*>), pure)
#endif

import Text.Namelist.Types

isLetter :: Char -> Bool
isLetter i
    | '\97' <= i && i <= '\122' = True
    | '\65' <= i && i <=  '\90' = True
    | otherwise                 = False

letter :: Stream s m Char => ParsecT s u m Char
letter = satisfy isLetter <?> "letter"

isAlphaNumeric :: Char -> Bool
isAlphaNumeric a = isLetter a || isDigit a || a == '_'

alphanumericCharacter :: Stream s m Char => ParsecT s u m Char
alphanumericCharacter = satisfy isAlphaNumeric <?> "alphanumeric-character"

name :: Stream s m Char => ParsecT s u m (CI String)
name = do
    n <- (:) <$> letter <*> many alphanumericCharacter <?> "name"
    if length n > 31
        then fail "name too long"
        else return $ mk n

sign :: (Stream s m Char, Num a) => ParsecT s u m (a -> a)
sign = (id <$ char '+') <|> (negate <$ char '-') <?> "sign"

fDigit :: Stream s m Char => ParsecT s u m Int
fDigit = (-) <$> (fromEnum <$> digit) <*> pure 48 <?> "digit"

unsignedInteger :: Stream s m Char => ParsecT s u m Int
unsignedInteger = chainl1 fDigit (pure $ \s d -> 10 * s + d)

integerLiteral :: Stream s m Char => ParsecT s u m Int
integerLiteral = ($) <$> option id sign <*> unsignedInteger <?> "integer"

exponentialPart :: Stream s m Char => ParsecT s u m Int
exponentialPart = oneOf "eEdDqQ" *> integerLiteral

realExpLiteral :: Stream s m Char => ParsecT s u m Double
realExpLiteral = do
    s <- option id sign
    i <- fromIntegral <$> unsignedInteger
    e <- fromIntegral <$> exponentialPart
    return $ s (i * 10 ** e)

realDotLiteral :: Stream s m Char => ParsecT s u m Double
realDotLiteral = do
    sgn    <- option id sign
    mbi    <- optionMaybe unsignedInteger
    _      <- char '.'
    (f, e) <- chainl ((,) <$> (fromIntegral <$> fDigit) <*> pure 1)
        (pure $ \(s, n) (d, _) -> (10 * s + d, n + 1)) (0 :: Integer,0)
    e2     <- option 0 exponentialPart
    i <- case mbi of
        Nothing | e == 0    -> fail "either decimal and floating part are missing"
                | otherwise -> return 0
        Just i -> return i

    return $ sgn $ fromIntegral (fromIntegral i * 10 ^ e + f) / 10 ** (fromIntegral $ e - e2)

realLiteral :: Stream s m Char => ParsecT s u m Double
realLiteral = try realExpLiteral <|> realDotLiteral

tokenize :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
tokenize p = spaces *> p <* spaces

complexLiteral :: Stream s m Char => ParsecT s u m (Complex Double)
complexLiteral = (:+)
    <$> (char '(' *> tokenize realLiteral)
    <*> (char ',' *> tokenize realLiteral <* char ')')

ciString :: Stream s m Char => String -> ParsecT s u m String
ciString []     = return []
ciString (a:as) = (:) <$> oneOf [toUpper a, toLower a] <*> ciString as

shortLogicalLiteral :: Stream s m Char => ParsecT s u m Bool
shortLogicalLiteral = choice
    [ True  <$ (char 'T' <|> char 't')
    , False <$ (char 'F' <|> char 'f')
    ] <* lookAhead (satisfy $ not . isAlphaNumeric)

longLogicalLiteral :: Stream s m Char => ParsecT s u m Bool
longLogicalLiteral = char '.' *> choice
    [ True  <$ ciString "true."
    , False <$ ciString "false."
    ]

logicalLiteral :: Stream s m Char => ParsecT s u m Bool
logicalLiteral = shortLogicalLiteral <|> longLogicalLiteral

stringLiteral :: Stream s m Char => ParsecT s u m String
stringLiteral = sl '\'' <|> sl '"'
  where
    sl   s = char s *> many (body s) <* char s
    body s = try (s <$ char s *> char s) <|> noneOf [s]

literal :: Stream s m Char => ParsecT s u m Value
literal = choice
    [ try mulValue
    , try mulNull
    , try $ Real <$> realLiteral
    , Integer    <$> integerLiteral
    , Complex    <$> complexLiteral
    , try $ Logical <$> logicalLiteral
    , String     <$> stringLiteral
    ]

mulNull :: Stream s m Char => ParsecT s u m Value
mulNull = (:* Null) <$> unsignedInteger <* char '*'

mulValue :: Stream s m Char => ParsecT s u m Value
mulValue = (:*) <$> unsignedInteger <*> (char '*' *> literal)

index :: Stream s m Char => ParsecT s u m Index
index = choice
    [ try $ Range <$> optionMaybe u <*> s (optionMaybe u) <*> s (optionMaybe i)
    , try $ Range <$> optionMaybe u <*> s (optionMaybe u) <*> pure Nothing
    , try $ Index <$> u
    ] <?> "index"
  where
    u = tokenize unsignedInteger
    i = tokenize integerLiteral
    s = (char ':' *>)

key :: Stream s m Char => ParsecT s u m Key
key = choice
    [ try $ Indexed <$> name <*> (char '(' *> sepBy1 index (char ',') <* char ')')
    , try $ Sub <$> name <*> (char '%' *> key)
    , try $ Key <$> name
    ] <?> "key"

keyVal :: Stream s m Char => ParsecT s u m Pair
keyVal = do
    k <- key
    _ <- tokenize $ char '='
    v <- tokenize (literal <|> pure Null) `sepEndBy` tokenize (char ',')
    return $ k := case reverse v of
        [a]          -> a
        [Null,a]     -> a
        Null:Null:as -> Array (reverse $ Null:as)
        Null:as      -> Array (reverse as)
        as           -> Array (reverse as)

group :: Stream s m Char => ParsecT s u m Group
group = Group <$> tokenize (char '&' *> name) <*> many keyVal <* tokenize (char '/')

namelist :: Stream s m Char => ParsecT s u m [Group]
namelist = many group

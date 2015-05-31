{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Instances where

import Control.Monad(replicateM)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative((<$>), (<*>), pure)
#endif

import Test.Tasty.QuickCheck
import Text.Namelist.Types
import Data.Complex(Complex((:+)))
import Data.CaseInsensitive(mk)
import Data.Char(toUpper)
import Numeric(showFFloat)

arbitrarySafeDouble :: Gen Double
arbitrarySafeDouble = suchThat (arbitrary :: Gen Double) $ \d ->
    let (n, _:f) = break (== '.') $ showFFloat Nothing d ""
    in d == read (n ++ f) / 10 ** fromIntegral (length f)

arbitraryIndex :: Gen Index
arbitraryIndex = do
    i <- choose (1, maxBound)
    return $ Index i

arbitraryRange :: Gen Index
arbitraryRange = do
    a <- choose (1, maxBound - 1)
    b <- choose (a + 1, maxBound)
    return $ Range a b

arbitraryStep :: Gen Index
arbitraryStep = do
    Range a b <- arbitraryRange
    s <- arbitrary
    return $ Step a b s

instance Arbitrary Index where
    arbitrary = oneof
        [ arbitraryIndex
        , arbitraryRange
        , arbitraryStep
        ]

arbitraryName :: Gen String
arbitraryName = sized $ \i -> do
    a  <- elements alpha
    as <- replicateM (min i 30) (elements an_)
    return $ a : as
  where
    an_   = '_': ['0' .. '9'] ++ alpha
    alpha = map toUpper lower ++ lower
    lower = ['a'..'z']

arbitraryKeyName :: Gen Key
arbitraryKeyName = oneof
    [ Key . mk <$> arbitraryName
    , Indexed  <$> (mk <$> arbitraryName) <*> listOf1 arbitrary
    ]

arbitrarySub :: Gen Key
arbitrarySub = sized $ \depth ->
    if depth < 1
    then arbitraryKeyName
    else Sub <$> (mk <$> arbitraryName) <*> arbitrary

instance Arbitrary Key where
    arbitrary = oneof
        [ arbitraryKeyName
        , arbitrarySub
        ]

newtype Scalar = Scalar { unScalar :: Value }

instance Arbitrary Scalar where
    arbitrary = Scalar <$> oneof
        [ Integer <$> arbitrary
        , Real    <$> arbitrarySafeDouble
        , Complex <$> ((:+) <$> arbitrarySafeDouble <*> arbitrarySafeDouble)
        , Logical <$> arbitrary
        , String  <$> arbitrary
        ]

instance Arbitrary Value where
    arbitrary = oneof
        [ unScalar <$> arbitrary
        , pure Null
        , sized $ \i -> do
            as <- map unScalar <$> replicateM (max 1 i) arbitrary
            a  <- unScalar <$> arbitrary
            return $ Array (reverse $ a:as)
        , sized $ \i -> (:*) <$> pure i <*> (unScalar <$> arbitrary)
        ]

instance Arbitrary Pair where
    arbitrary = (:=) <$> arbitrary <*> arbitrary

scaleDouble :: (Double -> Double) -> Gen a -> Gen a
scaleDouble f = scale (round . f . fromIntegral)

instance Arbitrary Group where
    arbitrary = Group <$> (mk <$> arbitraryName) <*> arbitrary

newtype Namelist = Namelist { getNamelist :: [Group] }

instance Show Namelist where
    show (Namelist gs) = show gs

instance Arbitrary Namelist where
    arbitrary = Namelist <$> scaleDouble sqrt arbitrary

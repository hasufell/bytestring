-- |
-- Module      : Properties.ShortByteString
-- Copyright   : (c) Julian Ospald 2022
-- License     : BSD-style

{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- We are happy to sacrifice optimizations in exchange for faster compilation,
-- but need to test rewrite rules. As one can check using -ddump-rule-firings,
-- rewrite rules do not fire in -O0 mode, so we use -O1, but disable almost all
-- optimizations. It roughly halves compilation time.
{-# OPTIONS_GHC -O1 -fenable-rewrite-rules
  -fmax-simplifier-iterations=1 -fsimplifier-phases=0
  -fno-call-arity -fno-case-merge -fno-cmm-elim-common-blocks -fno-cmm-sink
  -fno-cpr-anal -fno-cse -fno-do-eta-reduction -fno-float-in -fno-full-laziness
  -fno-loopification -fno-specialise -fno-strictness #-}

module Properties.ShortByteString (tests) where

import qualified Data.ByteString.Short as B
import Data.ByteString.Short (ShortByteString)

import Data.Word

import Control.Arrow
import Data.Foldable
import Data.List as L
import Data.Semigroup
import Data.Tuple
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
import Text.Show.Functions ()


-- from word8 package

isSpace :: Word8 -> Bool
isSpace w = w == _space
         || w == _tab
         || w == _lf
         || w == _cr
         || w == _np
         || w == _vt
         || w == _nbsp

_nul, _tab, _lf, _vt, _np, _cr :: Word8
_nul = 0x00
_tab = 0x09
_lf  = 0x0a
_vt  = 0x0b
_np  = 0x0c
_cr  = 0x0d

_nbsp :: Word8
_nbsp = 0xa0

_space :: Word8
_space = 0x20


-------

toElem :: Word8 -> Word8
toElem = id

swapW :: Word8 -> Word8
swapW = id


sizedByteString :: Int -> Gen ShortByteString
sizedByteString n = do m <- choose(0, n)
                       fmap B.pack $ vectorOf m arbitrary

instance Arbitrary ShortByteString where
  arbitrary = do
    bs <- sized sizedByteString
    n  <- choose (0, 2)
    return (B.pack . drop n . B.unpack $ bs) -- to give us some with non-0 offset
  shrink = map B.pack . shrink . B.unpack

instance CoArbitrary ShortByteString where
  coarbitrary s = coarbitrary (B.unpack s)

tests :: [TestTree]
tests =
  [ testProperty "pack . unpack" $
    \x -> x === B.pack (B.unpack x)
  , testProperty "unpack . pack" $
    \(map toElem -> xs) -> xs === B.unpack (B.pack xs)
  , testProperty "read . show" $
    \x -> (x :: ShortByteString) === x

  , testProperty "==" $
    \x y -> (x == y) === (B.unpack x == B.unpack y)
  , testProperty "== refl" $
    \x -> (x :: ShortByteString) == x
  , testProperty "== symm" $
    \x y -> ((x :: ShortByteString) == y) === (y == x)
  , testProperty "== pack unpack" $
    \x -> x == B.pack (B.unpack x)
  ]

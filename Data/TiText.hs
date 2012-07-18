{-# LANGUAGE OverloadedStrings #-}
-- | Parser and builder for a TI_TEXT data
-- parser reads a TEXT stream and output TI_TEXT data
-- As TI_TEXT file doesn't have end parser produces partial result
-- and should be forced in order to get DONE:
--
-- > feed (parse parser data) Data.Text.empty
--
-- To use with stream libraries such as conduit or iteratee 
-- tiBlock can be used.
--
-- TODO: add support for optional end mark Q
module Data.TiText
  ( -- * Parsers
    tiTextParser
  , tiBlockParser
    -- * Data builder
  , tiTextBuilderSimple
  , makeTiText
    -- * Serializer
  , tiTextSerialize
  ) where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.ByteString        (ByteString)
import           Data.Monoid
import           Data.Text.Lazy (Text)
import qualified Data.ByteString as S
import qualified Data.List as L
import qualified Data.Serialize.Builder as B
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import           Data.Word
import           Data.TiText.Types
-- import           Debug.Trace

-- | read byte
byte :: Parser Word8
byte = skipSpace >> hexadecimal <?> "byte"

-- | read TI_TEXT block
tiBlockParser :: Parser TiBlock
tiBlockParser = do
  skipWhile (/= '@')
  _ <- char '@'
  addr <- hexadecimal
  dat  <- foldrMany bpack B.empty byte
  return (addr,B.toByteString dat)
  where
    bpack :: Word8 -> B.Builder -> B.Builder
    bpack = B.append . B.singleton

-- | parse all TI_TEXT blocks
tiTextParser :: Parser TiText
tiTextParser = skipWhile (/= '@') >> many1 tiBlockParser

-- | create TiText from a data
makeTiText :: Int -> ByteString -> TiText
makeTiText i b = snd $ L.mapAccumL (\s d -> (s+S.length d,(s,d))) 0 $ splitBy i b

-- | Serialize TI_TEXT internal representation to Text
tiTextSerialize :: TiText -> Text
tiTextSerialize = TB.toLazyText . foldr (mappend) mempty .  map toBuilder
  where
    toBuilder (a,d) = al `mappend` (TB.hexadecimal a `mappend` (el `mappend` dataToText d))
    strToText       = S.foldr toB el
    dataToText d    = foldr mappend mempty $ map strToText $ splitBy 16 d
    toB w b = TB.hexadecimal w `mappend` (sl `mappend` b)
    al = TB.singleton '@'
    el = TB.singleton '\n'
    sl = TB.singleton ' '


tiTextBuilderSimple :: Word8 -> TiText -> ByteString
tiTextBuilderSimple b = B.toByteString . foldr (B.append . B.fromByteString) B.empty . fix 0
  where 
    fix :: Int -> [TiBlock] -> [ByteString]
    fix _ [] = []
    fix a x@((a',d):xs) | a < a'    = let d' = S.replicate (a'-a) b in d':fix a' x
                        | a > a'    = let d' = S.drop (a'-a) d in d':fix (a+S.length d') xs
                        | otherwise = d:fix (a'+S.length d) xs

foldrMany :: (a -> b -> b) -> b -> (Parser a) -> Parser b
foldrMany f x p = foldr f x <$> (many p)

splitBy :: Int -> ByteString -> [ByteString]
splitBy i = go
  where
      go b | S.null b = []
           | otherwise = let (l,r) = S.splitAt i b in l:go r


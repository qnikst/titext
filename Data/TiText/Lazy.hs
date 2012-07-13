{-# LANGUAGE OverloadedStrings #-}
module Data.TiText.Lazy 
  ( -- * parser function
    parseTiText
    -- * convert functions
    -- ** simple functions
  , fromTiTextSimple
  , toTiTextSimple
    -- * helpers
  , writeTiTextSimple
  , readTiTextSimple
  )
  where

import Prelude hiding (FilePath)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as SL
import Data.Binary.Builder as B
import Data.Binary.Put
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy.Read as TR
import Data.Word
import Data.Int

import Filesystem.Path.CurrentOS
import Filesystem

import Data.TiText.Internal

parseTiText :: Text -> TiText
parseTiText = go . T.words
  where
    -- read address 
    go :: [Text] -> TiText
    go [] = []
    go (x:xs) = case T.head x of
                  '@' -> let ea   = TR.hexadecimal $ T.tail x
                             (d,xs') = go' B.empty xs
                             d' = runPut $! putBuilder d
                         in case ea of 
                              Right (a,_) -> (a,d'):go xs'
                              Left e  ->  error e  -- TODO: change logic
                  t | t `elem` "qQ" -> []
                    | otherwise     -> go xs -- skip 
    go' :: Builder -> [Text] -> (Builder,[Text])
    go' b [] = (b, [])
    go' b xa@(x:xs) = case T.head x of
                     '@' -> (b,xa)
                     t | t `elem` "qQ" -> (b,[])
                       | otherwise -> let ed = TR.hexadecimal $ T.tail x
                                      in case ed of
                                          Right (d,_) -> go' (b `B.append` (singleton d)) xs
                                          Left e -> error e


fromTiTextSimple :: TiText -> ByteString
fromTiTextSimple = go (0,B.empty) 
  where 
    go :: (Int64,Builder) -> TiText -> ByteString
    go (_,b) [] = runPut $! putBuilder b
    go (ad,b) ((ad',x):xs) | ad < ad' =  -- gap with zeros
      let z  = SL.replicate (ad' - ad) 0  -- TODO: use custom gap char
      in go (ad, b `B.append` (fromLazyByteString z)) ((ad,x):xs)
                           | ad' < ad =  -- take a part of bytestring
      go (ad,b) ((ad,SL.drop (ad-ad') x):xs) -- will work much better for strict bytestrings
                           | otherwise =
      go (ad + SL.length x, b `B.append` (fromLazyByteString x)) xs

toTiTextSimple :: (Maybe Int64) -> ByteString -> TiText
toTiTextSimple mi b = makeList $! span' mi
  where
    -- break bytestring into chunks
    span' Nothing  = [b]
    span' (Just i) = spanBy i b
    makeList xs = go 0 xs
      where
        go _ []     = []
        go i (y:ys) = let i' = SL.length y
                      in (i,y):go (i+i') ys

tiToText :: TiText -> Text
tiToText = T.unlines . map blockToText
  where
    blockToText :: TiBlock -> Text
    blockToText (a,d) = 
      let at = TB.singleton '@'
          el = TB.singleton '\n'
          a' = TB.hexadecimal a
          d' = foldr1 (mappend) $! map (d2b . SL.unpack) (spanBy 8 d)
          t  =  d' `seq` at `mappend` (a' `mappend` (el `mappend` d'))
      in TB.toLazyText t
    -- d2b is dataToBuilder
    d2b :: [Word8] -> TB.Builder
    d2b [] = TB.singleton '\n'
    d2b (x:xs) = 
      let b = d2b xs 
          x' = TB.hexadecimal x
      in b `seq` x' `mappend` (TB.singleton ' ' `mappend` b)
      


-- | read and convert TI_TEXT file to bytestring
-- this function uses simple converting function that assumes that
-- there is no overlaps in file.
readTiTextSimple :: FilePath -> IO ByteString
readTiTextSimple f = 
    readTextFile f >>= 
    return . parseTiText . T.fromStrict >>= 
    return . fromTiTextSimple

-- | convert bytestring to TI_TEXT file and write it to file
-- this function splits bytestring into chucks of size i, 
-- TODO: make gaps
writeTiTextSimple :: FilePath -> (Maybe Int64) -> ByteString -> IO ()
writeTiTextSimple f i t = 
  writeTextFile f (T.toStrict $! tiToText $ toTiTextSimple i t)


-- helpers
spanBy :: Int64 -> ByteString -> [ByteString]
spanBy i b | b == SL.empty = []
           | otherwise     = let (l,r) = SL.splitAt i b in l:spanBy i r


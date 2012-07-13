-- | Reading TI_TEXT files
module Data.TiText 
  ( readTiText
  , splitMerge
  , splitBlocks
  , mergeBlocks
  , module Data.TiText.Internal
  ) where

-- TODO:
--        move types to internal
--        create lazy interface
--        create text interface
--        mark string as deprecated
--        create bytestring interfaces  

import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.List
import Data.Word
import Data.Int
import Numeric
import Data.Binary.Put
import Data.Binary.Builder

import Data.TiText.Internal

{- |
  Load TiText file 
  FilePath - path to file

  If parse error occur error will be thrown
 -}
readTiText :: FilePath -> IO TiText
readTiText f = fmap getBlocks (readFile f)

splitMerge :: Word8 -> Int64 -> TiText -> TiText
splitMerge w i t = splitBlocks i [mergeBlocks w t]

getBlocks :: String -> TiText
getBlocks = go . words
  where 
    -- go empty' items
    go :: [String] -> TiText
    go []      = []
    go ("q":_) = [] -- end of the file
    go ("Q":_) = [] -- end of the file
    go (('@':a):xs) = 
      let (b,r) = go' empty xs -- start of the new block
      in  (readAddr a, runPut $ putBuilder b) : go r
    go' :: Builder -> [String] -> (Builder, [String])
    go' b [] =  (b,[])
    go' b ("q":_) = (b,[])
    go' b ("Q":_) = (b,[])
    go' b xs@(('@':_):_) = (b,xs)
    go' b (x:xs) = let w = fst . head . readHex $ x
                   in go' (b `append` singleton w) xs
    readAddr x | all isHexDigit x = fst . head . readHex $ x
               | otherwise        = error "unexpected symbol"
{-    getBlocks' :: [SBlock] -> SBlock -> [String] -> [SBlock]
    getBlocks' acc b []      = acc `snoc` b 
    getBlocks' acc b ("q":_) = acc `snoc` b
    getBlocks' acc b ("Q":_) = acc `snoc` b
    getBlocks' acc b (('@':x):xs) | all isHexDigit x = 
                  getBlocks' (revBlock b:acc) ( fst . head . readHex $ x, []) xs
    getBlocks' acc (s1,b) (x:xs) | all isHexDigit x = 
                  getBlocks' acc (s1, ( ( fst . head . readHex $ x):b )) xs
    getBlocks' _ _ _ = error "unexpected symbol"

    revBlock :: (a,[b]) -> (a, [b])
    revBlock = second reverse  -}

splitBlocks :: Int64 -> TiText -> TiText
splitBlocks size = foldl (splitter size) [] 

splitter :: Int64 -> TiText -> TiBlock -> TiText
splitter size acc (s,b) = acc ++ zip [ s, size+s..] (splitBy size b)
  where
    splitBy :: Int64 -> B.ByteString -> [B.ByteString]
    splitBy _ xs | xs==B.empty = []
    splitBy i xs = let (x, y) = B.splitAt i xs
                    in (x:splitBy i y)

mergeBlocks :: Word8 -> TiText -> TiBlock
mergeBlocks d b = let sb = sortBy ( \(x,_) (y,_) -> compare x y) b
  in foldl (merger d) (head sb) (tail sb)

merger :: Word8 -> TiBlock -> TiBlock -> TiBlock
merger def (aA,dA) (aB,dB) = (aA, dA `B.append` B.replicate diff def `B.append` dB)
  where d = aB-aA
        diff = d - B.length dA


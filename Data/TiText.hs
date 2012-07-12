-- | Reading TI_TEXT files
module Data.TiText 
  ( TiText 
  , readTiText
  , split
  , merge
  , splitMerge
  , splitBlocks
  , mergeBlocks
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char
import Data.List
import Data.Word
import Control.Arrow (second)
import Numeric

type Block  = (Int, ByteString)                 -- ^ Starting address of block and block data
type SBlock = (Int, [Word8]) 

type TiText = [Block] 

{- |
  Load TiText file 
  FilePath - path to file

  If parse error occur error will be thrown
 -}
readTiText :: FilePath -> IO TiText
readTiText f = fmap parseTiText (readFile f)

parseTiText :: String -> TiText
parseTiText = getBlocks

split :: Int -> TiText -> TiText
split = splitBlocks 

merge :: Word8 -> TiText -> TiText
merge w t = [ mergeBlocks w t]

splitMerge :: Word8 -> Int -> TiText -> TiText
splitMerge w i t = split i (merge w t)




getBlocks :: String -> TiText
getBlocks s = map (second B.pack) $ tail $ reverse $ getBlocks' [] (0,[]) (words s)
  where 
    getBlocks' :: [SBlock] -> SBlock -> [String] -> [SBlock]
    getBlocks' acc b []      = revBlock b:acc
    getBlocks' acc b ("q":_) = revBlock b:acc
    getBlocks' acc b ("Q":_) = revBlock b:acc
    getBlocks' acc b (('@':x):xs) | all isHexDigit x = 
                  getBlocks' (revBlock b:acc) ( fst . head . readHex $ x, []) xs
    getBlocks' acc (s1,b) (x:xs) | all isHexDigit x = 
                  getBlocks' acc (s1, ( ( fst . head . readHex $ x):b )) xs
    getBlocks' _ _ _ = error "unexpected symbol"

    revBlock :: (a,[b]) -> (a, [b])
    revBlock = second reverse  

splitBlocks :: Int -> TiText -> TiText
splitBlocks size = foldl (splitter size) [] 

splitter :: Int -> TiText -> Block -> TiText
splitter size acc (s,b) = acc ++ zip [ s, size+s..] (splitBy size b)
  where
    splitBy :: Int -> B.ByteString -> [B.ByteString]
    splitBy _ xs | xs==B.empty = []
    splitBy i xs = let (x, y) = B.splitAt i xs
                    in (x:splitBy i y)

mergeBlocks :: Word8 -> TiText -> Block
mergeBlocks d b = let sb = sortBy ( \(x,_) (y,_) -> compare x y) b
  in foldl (merger d) (head sb) (tail sb)

merger :: Word8 -> Block -> Block -> Block
merger def (aA,dA) (aB,dB) = (aA, dA `B.append` B.replicate diff def `B.append` dB)
  where d = aB-aA
        diff = d - B.length dA


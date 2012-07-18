module Data.TiText.Types where

import Data.ByteString

-- | data block
type TiBlock = (Int, ByteString) 

type TiText = [TiBlock]




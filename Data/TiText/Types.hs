-- | 
-- Module: Data.TiText.Types
-- License: BSD-3
-- Maintainer:  alexander.vershilov@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- A module representing inner TiText type
--
module Data.TiText.Types 
  ( TiText
  , TiBlock
  ) where

import Data.ByteString

-- | Data block offset and binary data
type TiBlock = (Int, ByteString) 

-- | Inner representation of TI_TEXT 
-- list of TiBlock
type TiText = [TiBlock]




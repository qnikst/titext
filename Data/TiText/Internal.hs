-- | TiText inner representation, currently TiText uses 
-- Lazy ByteString as inner type but it can be changed after 
-- benchmarks

module Data.TiText.Internal
  ( TiText 
  , TiBlock
  )
  where

import Data.Int
import Data.ByteString.Lazy (ByteString)

-- | internal representation of TI_TEXT type
type TiText = [ TiBlock ] 

-- | Block of TI_TEXT Data
type TiBlock = ( Int64                -- ^ starting address
               , ByteString           -- ^ inner data
               )

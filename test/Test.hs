import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit

import Test.HUnit
import Test.QuickCheck
import Data.Attoparsec.Text
import Data.Text.Lazy
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Data.TiText

main = defaultMain tests

tests = [ testGroup "tiBlockParser"
            [ testProperty "bytestring -> titext -> bytestring" prop_readSerialize ]
        , testGroup "tiTextParser" 
            [ testProperty "bytestring -> titext -> text -> titext -> bytestring" prop_readSerialize2 ]
        ]

instance Arbitrary ByteString where
    arbitrary = S.pack `fmap` arbitrary
--    coarbitrary = coarbitrary . S.unpack

prop_readSerialize b = tiTextBuilderSimple 0 (makeTiText 16 b) == b
  where types = (b::ByteString)

prop_readSerialize2 b = b' == b 
  where 
    titext1 = makeTiText 16 b
    text  = toStrict $ tiTextSerialize titext1
    titext2 = case parseOnly tiTextParser text of 
                Right t -> t
                Left  _ -> []
    b'    = tiTextBuilderSimple 0 titext2
    types = (b::ByteString)

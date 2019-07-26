{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDerivation.RandomSpec
    ( spec
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( xprv )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), DerivationType (..), Index, Passphrase (..), XPrv )
import Cardano.Wallet.Primitive.AddressDerivation.Common
    ( Key (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( deriveAccountPrivateKey, deriveAddressPrivateKey, generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDerivationSpec
    ()
import Control.Monad
    ( (<=<) )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase )
import Data.ByteString
    ( ByteString )
import Test.Hspec
    ( Expectation, Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..), Property, choose, property, vectorOf )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

spec :: Spec
spec = do
    describe "Random Address Derivation Properties" $ do
        it "Key derivation works for various indexes" $
            property prop_keyDerivation

    describe "Golden tests" $ do
        it "generateKeyFromSeed" $
            generateTest test1
    
{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}

prop_keyDerivation
    :: Passphrase "seed"
    -> Passphrase "encryption"
    -> Index 'Hardened 'AccountK
    -> Index 'Soft 'AddressK
    -> Property
prop_keyDerivation seed encPwd ix1 ix2 =
    addrXPrv `seq` property () -- NOTE Making sure this doesn't throw
  where
    rootXPrv = generateKeyFromSeed seed encPwd
    accXPrv = deriveAccountPrivateKey encPwd rootXPrv ix1
    addrXPrv = deriveAddressPrivateKey encPwd accXPrv ix2

{-------------------------------------------------------------------------------
                                  Golden tests
-------------------------------------------------------------------------------}

data GenerateKeyFromSeed = GenerateKeyFromSeed
    { seed :: Passphrase "seed"
    , pwd :: Passphrase "encryption"
    , rootKey :: Key 'RootK XPrv
    } deriving (Show, Eq)

generateTest :: GenerateKeyFromSeed -> Expectation
generateTest GenerateKeyFromSeed{..} =
    generateKeyFromSeed seed pwd `shouldBe` rootKey

test1 :: GenerateKeyFromSeed
test1 = GenerateKeyFromSeed
    { seed = pp "df3fecb0196b9ef71fae1f6db886954d4ff9914fa075c665de03f17ca698398a"
    , pwd = pp "4a87f05fe25a57c96ff5221863e61b91bcca566b853b616f55e5d2c18caa1a4c"
    -- fixme: this was not generated by the previous wallet code.
    , rootKey = xprv16 "8001a61113793d1f90a78eb49a5a12c49a3a606629bcde236cdd6724478245fdd671815607e8ae10491e68db54f4322ad3723e7de0d06a90754ac5cf581184fb9471cfd860d3d28af4e025eaee37faf2b5493822afee2002ae7eabfbb7f579c604da83ffe99c45e3e4a86e4914a75d98d6b5ad9f7c7b68e5836b02a1377fedc5"
    }

-- Get a private key from a hex string, without error checking.
xprv16 :: ByteString -> Key purpose XPrv
xprv16 hex = Key k
  where
    Right k = xprvFromText hex
    xprvFromText = xprv <=< fromHexText
    fromHexText :: ByteString -> Either String ByteString
    fromHexText = convertFromBase Base16

-- Get a passphrase from a hex string, without error checking
pp :: ByteString -> Passphrase purpose
pp hex = Passphrase b
    where Right b = convertFromBase Base16 hex


{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

instance {-# OVERLAPS #-} Arbitrary (Passphrase "seed") where
    arbitrary = do
        n <- choose (32, 64)
        bytes <- BS.pack <$> vectorOf n arbitrary
        return $ Passphrase $ BA.convert bytes

{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE BinaryLiterals       #-}
 
module Main where

import Cardano.Api           
  ( writeFileTextEnvelope
  , PlutusScriptVersion (..)
  , PlutusScriptV1
  , PlutusScriptV2
  , PlutusScriptV3
  , IsPlutusScriptLanguage (..)
  , Script (..)
  , ScriptHash (..)
  , hashScript
  , prettyPrintJSON)
import Cardano.Api.Shelley   
  ( File (..)
  , PlutusScript (..)
  , Script (..)
  , serialiseToRawBytes
  , fromPlutusData
  , scriptDataToJsonDetailedSchema
  , unsafeHashableScriptData)
import PlutusTx               (CompiledCode, liftCodeDef, unsafeApplyCode)
import qualified PlutusTx.Builtins  as PlutusTx
import qualified PlutusLedgerApi.V3 as PlutusV3
import qualified PlutusLedgerApi.V2 as PlutusV2
import qualified PlutusLedgerApi.V1 as PlutusV1

import ColdScripts                
  ( coldAlwaysTrueMintCode
  , coldLockScriptCode
  , coldCredentialScriptCode
  , X509 (..)
  , ColdLockScriptDatum (..)
  , ColdLockScriptRedeemer (..))
import HotScripts
  ( hotCredentialScriptCode
  , hotLockScriptCode
  , hotAlwaysTrueMintCode
  , HotLockScriptRedeemer (..))

import Data.Aeson (Value)
import GHC.ByteOrder ( ByteOrder(..) )
import qualified Data.ByteString.Char8 as BS8

writePlutusScriptToFile :: IsPlutusScriptLanguage lang => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script = writeFileTextEnvelope (File filePath) Nothing script >>= \case
  Left err -> print "error writing script"
  Right () -> putStrLn $ "Serialized script to: " ++ filePath

writeCodeToFile :: forall lang a. PlutusScriptVersion lang -> FilePath -> CompiledCode a -> IO ()
writeCodeToFile version filePath = case version of
  PlutusScriptV1 -> writePlutusScriptToFile @PlutusScriptV1 filePath . PlutusScriptSerialised . PlutusV1.serialiseCompiledCode
  PlutusScriptV2 -> writePlutusScriptToFile @PlutusScriptV2 filePath . PlutusScriptSerialised . PlutusV2.serialiseCompiledCode
  PlutusScriptV3 -> writePlutusScriptToFile @PlutusScriptV3 filePath . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode

---------------------------------------

pkh :: PlutusV3.PubKeyHash
pkh = "431ff8049ad5ee5f6c30bae16f28e9109355fe7de42432bfb9c44282"

-- coldAlwaysTrueMintCodeApplied :: CompiledCode (BuiltinData -> BuiltinData -> ())
coldAlwaysTrueMintCodeApplied = coldAlwaysTrueMintCode `unsafeApplyCode` liftCodeDef (PlutusV3.toBuiltinData pkh)

-- hotAlwaysTrueMintCodeApplied :: CompiledCode (BuiltinData -> BuiltinData -> ())
hotAlwaysTrueMintCodeApplied = hotAlwaysTrueMintCode `unsafeApplyCode` liftCodeDef (PlutusV3.toBuiltinData pkh)

scriptHashColdAlwaysTrueMint :: ScriptHash
scriptHashColdAlwaysTrueMint = hashScript . PlutusScript PlutusScriptV3 . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode $ coldAlwaysTrueMintCodeApplied

scriptHashHotAlwaysTrueMint :: ScriptHash
scriptHashHotAlwaysTrueMint = hashScript . PlutusScript PlutusScriptV3 . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode $ hotAlwaysTrueMintCodeApplied

coldAlwaysTrueCurrencySymbol :: PlutusV3.CurrencySymbol
coldAlwaysTrueCurrencySymbol = PlutusV3.CurrencySymbol . PlutusV3.toBuiltin . serialiseToRawBytes $ scriptHashColdAlwaysTrueMint

hotAlwaysTrueCurrencySymbol :: PlutusV3.CurrencySymbol
hotAlwaysTrueCurrencySymbol = PlutusV3.CurrencySymbol . PlutusV3.toBuiltin . serialiseToRawBytes $ scriptHashHotAlwaysTrueMint

dataToJSON :: PlutusV3.ToData a => a -> Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . PlutusV3.toData

printDataToJSON :: PlutusV3.ToData a => a -> IO ()
printDataToJSON = putStrLn . BS8.unpack . prettyPrintJSON . dataToJSON

main :: IO ()
main = do
  writeCodeToFile PlutusScriptV3 "./assets/V3/coldAlwaysTrueMint.plutus" coldAlwaysTrueMintCodeApplied
  writeCodeToFile PlutusScriptV3 "./assets/V3/hotAlwaysTrueMint.plutus" hotAlwaysTrueMintCodeApplied
  -- setting up the cold credential and locking scripts 
  let coldCredentialScriptCodeApplied = coldCredentialScriptCode `unsafeApplyCode` liftCodeDef (PlutusV3.toBuiltinData coldAlwaysTrueCurrencySymbol)
  putStrLn $ "Applied currency symbol " ++ show coldAlwaysTrueCurrencySymbol ++ " to coldCredentialScriptCode"
  -- writing cold side to file
  writeCodeToFile PlutusScriptV3 "./assets/V3/coldCredentialScript.plutus" coldCredentialScriptCodeApplied
  writeCodeToFile PlutusScriptV3 "./assets/V3/coldLockScript.plutus" coldLockScriptCode
  -- setting up the hot credential and locking scripts
  let hotCredentialScriptCodeApplied = hotCredentialScriptCode `unsafeApplyCode` liftCodeDef (PlutusV3.toBuiltinData hotAlwaysTrueCurrencySymbol)
  putStrLn $ "Applied currency symbol " ++ show hotAlwaysTrueCurrencySymbol ++ " to hotCredentialScriptCode"
  let hotLockScriptCodeApplied = hotLockScriptCode `unsafeApplyCode` liftCodeDef (PlutusV3.toBuiltinData coldAlwaysTrueCurrencySymbol)
  putStrLn $ "Applied currency symbol " ++ show coldAlwaysTrueCurrencySymbol ++ " to hotLockScriptCode"
  -- writing hot side to file
  writeCodeToFile PlutusScriptV3 "./assets/V3/hotCredentialScript.plutus" hotCredentialScriptCodeApplied
  writeCodeToFile PlutusScriptV3 "./assets/V3/hotLockScript.plutus" hotLockScriptCodeApplied
  -- printDataToJSON datums
  -- the initial datums for the e2e example for both the cold and hot locking scripts
  writeFile "./assets/datums/initColdLockScriptDatum.json" (BS8.unpack . prettyPrintJSON $ dataToJSON initColdLockScriptDatum)
  writeFile "./assets/datums/initHotLockScriptDatum.json" (BS8.unpack . prettyPrintJSON $ dataToJSON initHotLockScriptDatum)
  -- the delegate redeemer needed to authorize the hot credential
  writeFile "./assets/redeemers/delegateRedeemer.json" (BS8.unpack . prettyPrintJSON $ dataToJSON delegateRedeemer)
  -- the resign redeemer needed to remove a delegate from the old datum
  writeFile "./assets/datums/resignMemberChild4NewDatum.json" (BS8.unpack . prettyPrintJSON $ dataToJSON resignMemberChild4NewDatum)
  writeFile "./assets/redeemers/resignMemberChild4Redeemer.json" (BS8.unpack . prettyPrintJSON $ dataToJSON resignMemberChild4Redeemer)
  -- the recover redeemer needed to recover the CC NFT from lock script
  writeFile "./assets/redeemers/recoverColdRedeemer.json" (BS8.unpack . prettyPrintJSON $ dataToJSON recoverColdRedeemer)
  -- the vote redeemer needed witness the Vote NFT from lock script
  writeFile "./assets/redeemers/voteRedeemer.json" (BS8.unpack . prettyPrintJSON $ dataToJSON voteRedeemer)
  -- the resign redeemer needed to remove child 7 from the old datum
  writeFile "./assets/datums/resignMemberChild7NewDatum.json" (BS8.unpack . prettyPrintJSON $ dataToJSON resignMemberChild7NewDatum)
  -- the resign redeemer needed to remove child 7 from the old datum
  writeFile "./assets/redeemers/resignMemberChild7Redeemer.json" (BS8.unpack . prettyPrintJSON $ dataToJSON resignMemberChild7Redeemer)
  -- the recover redeemer needed to recover the Vote NFT from hot lock script
  writeFile "./assets/redeemers/recoverHotRedeemer.json" (BS8.unpack . prettyPrintJSON $ dataToJSON recoverHotRedeemer)
  putStrLn "done!"

initColdLockScriptDatum :: ColdLockScriptDatum
initColdLockScriptDatum = ColdLockScriptDatum {
    caX509 = caCert,
    recoveryX509s = [child1Cert, child2Cert, child3Cert],
    delegateX509s = [child4Cert, child5Cert, child6Cert]
}

initHotLockScriptDatum :: [X509]
initHotLockScriptDatum = [child7Cert, child8Cert, child9Cert]

resignMemberChild4NewDatum :: ColdLockScriptDatum
resignMemberChild4NewDatum = ColdLockScriptDatum {
    caX509 = caCert,
    recoveryX509s = [child1Cert, child2Cert, child3Cert],
    delegateX509s = [child5Cert, child6Cert]
}

resignMemberChild7NewDatum :: [X509]
resignMemberChild7NewDatum = [child8Cert, child9Cert]

resignMemberChild4Redeemer :: ColdLockScriptRedeemer
resignMemberChild4Redeemer = ColdScripts.Resign child4Cert

recoverColdRedeemer :: ColdLockScriptRedeemer
recoverColdRedeemer = ColdScripts.Recover

delegateRedeemer :: ColdLockScriptRedeemer
delegateRedeemer = ColdScripts.Delegate

voteRedeemer :: HotLockScriptRedeemer
voteRedeemer = HotScripts.Vote

resignMemberChild7Redeemer :: HotLockScriptRedeemer
resignMemberChild7Redeemer = HotScripts.Resign child7Cert

recoverHotRedeemer :: HotLockScriptRedeemer
recoverHotRedeemer = HotScripts.Recover

-- helper function to create a X509 certificate for testing purposes
-- the first argument is 28 bytes long and the second is 32 bytes long
createCert :: Integer -> Integer -> X509
createCert pubKeyHash x509Hash = X509 {
    pubKeyHash = PlutusV3.PubKeyHash (PlutusTx.integerToByteString BigEndian 28 pubKeyHash),
    x509Hash = PlutusTx.integerToByteString BigEndian 32 x509Hash
}

-- Some hardcoded X509 certificate from the x509-cert-example folder

caCert :: X509
caCert = createCert 0x09159adec41ce5d48dde24a275a5b2c2e79461c8693ef60af9fc3207 0x0ff1fd44947bcd4cdc6f06841d881ac2a0beb3f15ba5f5e3c08991d92e8ba643

-- 3 recovery keys

child1Cert :: X509
child1Cert = createCert 0xff7a6c9f3ebf80ab457cca7813842aa2150d0dad341a7956a334c76d 0x1a82818b488574c156f1fa8941bad9b4b4976ba21cfaede1ab33a30de39f7edd

child2Cert :: X509
child2Cert = createCert 0xc2233827cca3a0cc2c49f91a66276c468be994db855d6b413005fa88 0x3b8536a38eea871cc8b2775deb5861ac4348ef61a84b9e9c643480ae5b88ffc3

child3Cert :: X509
child3Cert = createCert 0xb23a02a308165c702ce00bf760a0eff33b27b12906e1805b7685125f 0xfdf913abfdb8f00997cca5c14ca0b82f3d08781015a061e91444425d6f777ffa

-- 3 delegation keys

child4Cert :: X509
child4Cert = createCert 0xfc6a114db76d31de585793749dcd6ad2d6c02a52ce9226820656bedd 0x7c9d1c732c313066ded1568dc24b1230cc782d331cb65465bc65ad5df6fbe832

child5Cert :: X509
child5Cert = createCert 0x168ff0600f6245812192fb84c1d5a72129ae0445a272acc65dc88fb3 0xc60e20be4ce0fa457a8c65ade01005475e71880e921c2ee40a6b51d42fd95e11

child6Cert :: X509
child6Cert = createCert 0xc530a8b72dd72e320e7f4883fcb98d0058e70efcf4e7e0871ce13eb7 0xce75748d37a55ef1faec7219708059479197965a5927a7f9901c6bc9707eeaa1

-- 3 voting keys

child7Cert :: X509
child7Cert = createCert 0xfb5e0be4801aea73135efe43f4a3a6d08147af523112986dd5e7d13b 0x57f5530e057e20b726b78aa31104d415cb2bce58c669829a44d009c1b1005bcd

child8Cert :: X509
child8Cert = createCert 0xa3c6cb93a32b02877c61f64ab1c66c4513f12788bf7c500ead7d941b 0x9923f31c1ce14e2acbd505fa8eebd4ce677d1bcd96c6d71610f810f2008ecc3a

child9Cert :: X509
child9Cert = createCert 0xeda6befbe1a4cb8191752d97b67627a548bcc5f3e4653ecfdba7cdf0 0xecd64beefcf59f01a975457b0a3623d2b03d5bcf71642a8d8d8275e4668aad31
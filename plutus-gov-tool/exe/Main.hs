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

import Scripts                
  ( alwaysTrueMintCode
  , lockingScriptCode
  , ccScriptCode
  , X509 (..)
  , CCScriptDatum (..)
  , CCScriptRedeemer (..))

import Data.Aeson             (Value)
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

scriptHashAlwaysTrueMint :: ScriptHash
scriptHashAlwaysTrueMint = hashScript . PlutusScript PlutusScriptV3 . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode $ alwaysTrueMintCode

alwaysTrueCurrencySymbol :: PlutusV3.CurrencySymbol
alwaysTrueCurrencySymbol = PlutusV3.CurrencySymbol . PlutusV3.toBuiltin . serialiseToRawBytes $ scriptHashAlwaysTrueMint

dataToJSON :: PlutusV3.ToData a => a -> Value
dataToJSON = scriptDataToJsonDetailedSchema . unsafeHashableScriptData . fromPlutusData . PlutusV3.toData

printDataToJSON :: PlutusV3.ToData a => a -> IO ()
printDataToJSON = putStrLn . BS8.unpack . prettyPrintJSON . dataToJSON

caCert :: X509
caCert = X509 {
    pubKeyHash = PlutusV3.PubKeyHash (PlutusTx.integerToByteString True 28 0xa798f5b18176daae143c8336452bb2d61b45d2fd835d45b77ad7594f),
    x509Hash = PlutusTx.integerToByteString True 32 0xe521c65c75b1557cf8df05417cc483352aeb169ea5ddb74cc56b285b18d8ba5
}

datum :: CCScriptDatum
datum = CCScriptDatum {
    recoveryX509s = [caCert],
    delegateX509s = [caCert]
}

datum2 :: CCScriptDatum
datum2 = CCScriptDatum {
    recoveryX509s = [caCert],
    delegateX509s = []
}

redeemer :: CCScriptRedeemer
redeemer = Recover

redeemer2 :: CCScriptRedeemer
redeemer2 = Resign caCert

main :: IO ()
main = do
  writeCodeToFile PlutusScriptV3 "./assets/V3/alwaysTrueMint.plutus" alwaysTrueMintCode
  let ccScriptCodeAplied = ccScriptCode `unsafeApplyCode` liftCodeDef (PlutusV3.toBuiltinData alwaysTrueCurrencySymbol)
  putStrLn $ "Applied currency symbol " ++ show alwaysTrueCurrencySymbol ++ " to ccScriptCode"
  writeCodeToFile PlutusScriptV3 "./assets/V3/ccScript.plutus" ccScriptCodeAplied
  writeCodeToFile PlutusScriptV3 "./assets/V3/lockingScript.plutus" lockingScriptCode
  -- printDataToJSON datum
  writeFile "./assets/V3/datum.json" (BS8.unpack . prettyPrintJSON $ dataToJSON datum)
  writeFile "./assets/V3/datum2.json" (BS8.unpack . prettyPrintJSON $ dataToJSON datum2)
  writeFile "./assets/V3/redeemer.json" (BS8.unpack . prettyPrintJSON $ dataToJSON redeemer)
  writeFile "./assets/V3/redeemer2.json" (BS8.unpack . prettyPrintJSON $ dataToJSON redeemer2)
  putStrLn "done!"
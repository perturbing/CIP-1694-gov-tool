{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
 
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
  , hashScript)
import Cardano.Api.Shelley   
  ( File (..)
  , PlutusScript (..)
  , Script (..)
  , serialiseToRawBytes)
import PlutusTx              (CompiledCode, liftCodeDef, unsafeApplyCode)
import qualified PlutusLedgerApi.V3 as PlutusV3
import qualified PlutusLedgerApi.V2 as PlutusV2
import qualified PlutusLedgerApi.V1 as PlutusV1

import ScriptsV3             (alwaysTrueMintCodeV3)
import ScriptsV2             (alwaysTrueMintCodeV2)
import ScriptsV1             (alwaysTrueMintCodeV1)

writePlutusScriptToFile :: IsPlutusScriptLanguage lang => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script = writeFileTextEnvelope (File filePath) Nothing script >>= \case
  Left err -> print "error writing script"
  Right () -> putStrLn $ "Serialized script to: " ++ filePath

writeCodeToFile :: forall lang a. PlutusScriptVersion lang -> FilePath -> CompiledCode a -> IO ()
writeCodeToFile version filePath = case version of
  PlutusScriptV1 -> writePlutusScriptToFile @PlutusScriptV1 filePath . PlutusScriptSerialised . PlutusV1.serialiseCompiledCode
  PlutusScriptV2 -> writePlutusScriptToFile @PlutusScriptV2 filePath . PlutusScriptSerialised . PlutusV2.serialiseCompiledCode
  PlutusScriptV3 -> writePlutusScriptToFile @PlutusScriptV3 filePath . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode

----------------------------------

scriptHashAlwaysTrueMintV3 :: ScriptHash
scriptHashAlwaysTrueMintV3 = hashScript . PlutusScript PlutusScriptV3 . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode $ alwaysTrueMintCodeV3

scriptHashAlwaysTrueMintV2 :: ScriptHash
scriptHashAlwaysTrueMintV2 = hashScript . PlutusScript PlutusScriptV2 . PlutusScriptSerialised . PlutusV2.serialiseCompiledCode $ alwaysTrueMintCodeV2

scriptHashAlwaysTrueMintV1 :: ScriptHash
scriptHashAlwaysTrueMintV1 = hashScript . PlutusScript PlutusScriptV1 . PlutusScriptSerialised . PlutusV1.serialiseCompiledCode $ alwaysTrueMintCodeV1

alwaysTrueCurrencySymbolV3 :: PlutusV3.CurrencySymbol
alwaysTrueCurrencySymbolV3 = PlutusV3.CurrencySymbol . PlutusV3.toBuiltin . serialiseToRawBytes $ scriptHashAlwaysTrueMintV3

alwaysTrueCurrencySymbolV2 :: PlutusV2.CurrencySymbol
alwaysTrueCurrencySymbolV2 = PlutusV2.CurrencySymbol . PlutusV2.toBuiltin . serialiseToRawBytes $ scriptHashAlwaysTrueMintV2

alwaysTrueCurrencySymbolV1 :: PlutusV1.CurrencySymbol
alwaysTrueCurrencySymbolV1 = PlutusV1.CurrencySymbol . PlutusV1.toBuiltin . serialiseToRawBytes $ scriptHashAlwaysTrueMintV1

main :: IO ()
main = do
  writeCodeToFile PlutusScriptV3 "./assets/V3/alwaysTrueMint.plutus" alwaysTrueMintCodeV3
  putStrLn $ "V3 currency symbol of always True script: " ++ show alwaysTrueCurrencySymbolV3
  writeCodeToFile PlutusScriptV2 "./assets/V2/alwaysTrueMint.plutus" alwaysTrueMintCodeV2
  putStrLn $ "V2 currency symbol of always True script: " ++ show alwaysTrueCurrencySymbolV2
  writeCodeToFile PlutusScriptV1 "./assets/V1/alwaysTrueMint.plutus" alwaysTrueMintCodeV1
  putStrLn $ "V1 currency symbol of always True script: " ++ show alwaysTrueCurrencySymbolV1
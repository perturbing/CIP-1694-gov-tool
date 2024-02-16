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
import PlutusTx               (CompiledCode, liftCodeDef, unsafeApplyCode)
import qualified PlutusLedgerApi.V3 as PlutusV3
import qualified PlutusLedgerApi.V2 as PlutusV2
import qualified PlutusLedgerApi.V1 as PlutusV1

import Scripts                (ccScriptCode, lockingScriptCode, alwaysTrueMintCode )
import qualified PlutusLedgerApi.V1 as V3

writePlutusScriptToFile :: IsPlutusScriptLanguage lang => FilePath -> PlutusScript lang -> IO ()
writePlutusScriptToFile filePath script = writeFileTextEnvelope (File filePath) Nothing script >>= \case
  Left err -> print "error writing script"
  Right () -> putStrLn $ "Serialized script to: " ++ filePath

writeCodeToFile :: forall lang a. PlutusScriptVersion lang -> FilePath -> CompiledCode a -> IO ()
writeCodeToFile version filePath = case version of
  PlutusScriptV1 -> writePlutusScriptToFile @PlutusScriptV1 filePath . PlutusScriptSerialised . PlutusV1.serialiseCompiledCode
  PlutusScriptV2 -> writePlutusScriptToFile @PlutusScriptV2 filePath . PlutusScriptSerialised . PlutusV2.serialiseCompiledCode
  PlutusScriptV3 -> writePlutusScriptToFile @PlutusScriptV3 filePath . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode

scriptHashAlwaysTrueMint :: ScriptHash
scriptHashAlwaysTrueMint = "0a1fa5015c69f9535c4c984a6fcb7c16b4f33cd52ffc840488f8459b"
-- scriptHashAlwaysTrueMint = hashScript . PlutusScript PlutusScriptV3 . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode $ alwaysTrueMintCode

alwaysTrueCurrencySymbol :: PlutusV3.CurrencySymbol
alwaysTrueCurrencySymbol = PlutusV3.CurrencySymbol . PlutusV3.toBuiltin . serialiseToRawBytes $ scriptHashAlwaysTrueMint

main :: IO ()
main = do
  writeCodeToFile PlutusScriptV3 "./assets/V3/alwaysTrueMint.plutus" alwaysTrueMintCode
  let ccScriptCodeAplied = ccScriptCode `unsafeApplyCode` liftCodeDef (V3.toBuiltinData alwaysTrueCurrencySymbol)
  putStrLn $ "Applied currency symbol " ++ show alwaysTrueCurrencySymbol ++ " to ccScriptCode"
  writeCodeToFile PlutusScriptV3 "./assets/V3/ccScript.plutus" ccScriptCodeAplied
  writeCodeToFile PlutusScriptV3 "./assets/V3/lockingScript.plutus" lockingScriptCode
  putStrLn "done!"
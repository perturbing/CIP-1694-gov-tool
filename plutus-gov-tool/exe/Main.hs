{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
 
module Main where

import Cardano.Api           (writeFileTextEnvelope, PlutusScriptV3, PlutusScriptV2)
import Cardano.Api.Shelley   (File (..), PlutusScript (..), Script (..))
import PlutusTx              (CompiledCode, liftCodeDef, unsafeApplyCode)
import ScriptsV3             (ccScriptCode, lockingScriptCode)
import ScriptsV2             (ccScriptCodeV2, lockingScriptCodeV2, alwaysTrueMintCodeV2)
import qualified PlutusLedgerApi.V3 as PlutusV3
import qualified PlutusLedgerApi.V2 as PlutusV2

writeScriptToFileV3 :: FilePath -> PlutusScript PlutusScriptV3 -> IO ()
writeScriptToFileV3 filePath script =
  writeFileTextEnvelope (File filePath) Nothing script >>= \case
    Left err -> print "error writing script"
    Right () -> putStrLn $ "Serialized script to: " ++ filePath

writeScriptToFileV2 :: FilePath -> PlutusScript PlutusScriptV2 -> IO ()
writeScriptToFileV2 filePath script =
  writeFileTextEnvelope (File filePath) Nothing script >>= \case
    Left err -> print "error writing script"
    Right () -> putStrLn $ "Serialized script to: " ++ filePath

-- Create file with compiled code
writeCodeToFileV3 :: FilePath -> CompiledCode a -> IO ()
writeCodeToFileV3 filePath = writeScriptToFileV3 filePath . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode

-- Create file with compiled code
writeCodeToFileV2 :: FilePath -> CompiledCode a -> IO ()
writeCodeToFileV2 filePath = writeScriptToFileV2 filePath . PlutusScriptSerialised . PlutusV2.serialiseCompiledCode

----------------------------------

currencySymbol :: PlutusV2.CurrencySymbol
currencySymbol = "4c4c8c78d1791f4ad31ffcb6a19d918f0393c3a1ade85885d518be65"

main :: IO ()
main = do
  -- V3
  writeCodeToFileV3 "./assets/V3/ccScript.plutus" ccScriptCode
  writeCodeToFileV3 "./assets/V3/lockingScript.plutus" lockingScriptCode
  -- V2
  writeCodeToFileV2 "./assets/V2/alwaysTrueMint.plutus" alwaysTrueMintCodeV2
  -- Apply always true minting policy to ccScript
  let appliedScriptV2 = ccScriptCodeV2 `unsafeApplyCode` liftCodeDef currencySymbol
  putStrLn $ "Applied currency symbol to script: " ++ show currencySymbol
  writeCodeToFileV2 "./assets/V2/ccScript.plutus" appliedScriptV2
  writeCodeToFileV2 "./assets/V2/lockingScript.plutus" lockingScriptCodeV2
  putStrLn "Done."
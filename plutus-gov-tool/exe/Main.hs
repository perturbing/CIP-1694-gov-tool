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
  , IsPlutusScriptLanguage (..))
import Cardano.Api.Shelley   (File (..), PlutusScript (..), Script (..) )
import PlutusTx              (CompiledCode, liftCodeDef, unsafeApplyCode)
import qualified PlutusLedgerApi.V3 as PlutusV3
import qualified PlutusLedgerApi.V2 as PlutusV2
import qualified PlutusLedgerApi.V1 as PlutusV1

import ScriptsV3             (ccScriptCode, lockingScriptCode)
import ScriptsV2             (ccScriptCodeV2, lockingScriptCodeV2, alwaysTrueMintCodeV2)

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

currencySymbol :: PlutusV2.CurrencySymbol
currencySymbol = "4c4c8c78d1791f4ad31ffcb6a19d918f0393c3a1ade85885d518be65"

main :: IO ()
main = do
  -- V3 (we can run this once the ledger implements the V3 Context)
  writeCodeToFile PlutusScriptV3 "./assets/V3/ccScript.plutus" ccScriptCode
  writeCodeToFile PlutusScriptV3 "./assets/V3/lockingScript.plutus" lockingScriptCode
  -- V2
  writeCodeToFile PlutusScriptV2 "./assets/V2/alwaysTrueMint.plutus" alwaysTrueMintCodeV2
  -- Apply always true minting policy to ccScript
  let appliedScriptV2 = ccScriptCodeV2 `unsafeApplyCode` liftCodeDef currencySymbol
  putStrLn $ "Applied currency symbol to script: " ++ show currencySymbol
  writeCodeToFile PlutusScriptV2 "./assets/V2/ccScript.plutus" appliedScriptV2
  writeCodeToFile PlutusScriptV2 "./assets/V2/lockingScript.plutus" lockingScriptCodeV2
  putStrLn "Done."
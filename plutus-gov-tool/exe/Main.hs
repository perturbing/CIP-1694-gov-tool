{-# LANGUAGE LambdaCase #-}

module Main where

import Cardano.Api           (writeFileTextEnvelope, PlutusScriptV3, PlutusScriptV2)
import Cardano.Api.Shelley   (File (..), PlutusScript (..), Script (..))
import PlutusTx              (CompiledCode)
import Scripts               (ccScriptCode, lockingScriptCode)
import ScriptsV2             (ccScriptCodeV2, lockingScriptCodeV2)
import qualified PlutusLedgerApi.V3 as PlutusV3
import qualified PlutusLedgerApi.V2 as PlutusV2

writeScriptToFile :: FilePath -> PlutusScript PlutusScriptV3 -> IO ()
writeScriptToFile filePath script =
  writeFileTextEnvelope (File filePath) Nothing script >>= \case
    Left err -> print "error writing script"
    Right () -> putStrLn $ "Serialized script to: " ++ filePath

writeScriptToFileV2 :: FilePath -> PlutusScript PlutusScriptV2 -> IO ()
writeScriptToFileV2 filePath script =
  writeFileTextEnvelope (File filePath) Nothing script >>= \case
    Left err -> print "error writing script"
    Right () -> putStrLn $ "Serialized script to: " ++ filePath

-- Create file with compiled code
writeCodeToFile :: FilePath -> CompiledCode a -> IO ()
writeCodeToFile filePath = writeScriptToFile filePath . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode

-- Create file with compiled code
writeCodeToFileV2 :: FilePath -> CompiledCode a -> IO ()
writeCodeToFileV2 filePath = writeScriptToFileV2 filePath . PlutusScriptSerialised . PlutusV2.serialiseCompiledCode

main :: IO ()
main = do
  writeCodeToFile "./assets/ccScript.plutus" ccScriptCode
  writeCodeToFile "./assets/lockingScript.plutus" lockingScriptCode
  writeCodeToFileV2 "./assets/ccScriptV2.plutus" ccScriptCodeV2
  writeCodeToFileV2 "./assets/lockingScriptV2.plutus" lockingScriptCodeV2
  putStrLn "Done."
{-# LANGUAGE LambdaCase #-}

module Main where

import Cardano.Api           (writeFileTextEnvelope, PlutusScriptV3)
import Cardano.Api.Shelley   (File (..), PlutusScript (..), Script (..))
import PlutusTx              (CompiledCode)
import Script                (ccScriptCode)
import qualified PlutusLedgerApi.V3 as PlutusV3

writeScriptToFile :: FilePath -> PlutusScript PlutusScriptV3 -> IO ()
writeScriptToFile filePath script =
  writeFileTextEnvelope (File filePath) Nothing script >>= \case
    Left err -> print "error writing script"
    Right () -> putStrLn $ "Serialized script to: " ++ filePath

-- Create file with compiled code
writeCodeToFile :: FilePath -> CompiledCode a -> IO ()
writeCodeToFile filePath = writeScriptToFile filePath . PlutusScriptSerialised . PlutusV3.serialiseCompiledCode

main :: IO ()
main = do
  writeCodeToFile "./assets/ccScript.plutus" ccScriptCode
  putStrLn "Done."
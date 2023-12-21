{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE Strict            #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Script where

import PlutusTx.Prelude 
    ( Bool(..), 
      BuiltinData, 
      map, 
      (.), 
      elem, 
      ($), 
      Foldable (foldr), 
      (||) )
import PlutusTx.Builtins (BuiltinData)
import PlutusTx.AssocMap (member)
import PlutusLedgerApi.V3
    ( TxOutRef(..),
      ScriptContext(..),
      CurrencySymbol,
      TxInfo(txInfoInputs), 
      TxInInfo (..), 
      ScriptPurpose (..), 
      TxOut (..), 
      Value (..) )
import PlutusTx (compile, CompiledCode)

-- [General notes on this file]
-- This file contains two plutus scripts, the CC membership script and the locking script. 
-- The CC membership script is parameterized by the currency symbol of an NFT, and validates 
-- true if the NFT is in a transaction input. The locking script governs the unlocking of this NFT,
-- and therefore when a CC certificated can be witnessed via the CC membership script.

-- [CC membership script]
-- In this script we first check that the usage of this script is for certifying.
-- We then check that the currency symbol of the hardcoded NFT is in a transaction inputs.
{-# INLINABLE ccScript #-}
ccScript :: CurrencySymbol -> () -> ScriptContext -> Bool
ccScript symbol _ ctx = case scriptContextPurpose ctx of
    Certifying _ -> foldr (\value acc -> acc || (symbol `member` value)) False txInputsValues
        where
            -- the list of transaction inputs being consumed in this transaction.
            txInputs = txInfoInputs . scriptContextTxInfo $ ctx
            -- the value maps of the transaction inputs.
            txInputsValues = map (getValue . txOutValue . txInInfoResolved) txInputs
    _            -> False

ccScriptCode :: CompiledCode (CurrencySymbol -> () -> ScriptContext -> Bool)
ccScriptCode = $$(compile [|| ccScript ||])
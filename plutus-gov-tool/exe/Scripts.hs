{-# LANGUAGE TemplateHaskell                    #-}
{-# LANGUAGE NamedFieldPuns                     #-}
{-# LANGUAGE DataKinds                          #-}
{-# LANGUAGE NoImplicitPrelude                  #-}
{-# LANGUAGE Strict                             #-}
{-# OPTIONS_GHC -O0                             #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas   #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}

module Scripts where

import PlutusTx.Prelude
    ( (.),
      ($),
      length,
      even,
      divide,
      Ord (..), Eq (..), Maybe (..), maybe, (<$>), (/=) )
import PlutusTx.List
    ( any,
      map,
      elem,
      foldr,
      filter, find )
import PlutusTx.Builtins
    ( BuiltinByteString, Integer, error )
import PlutusTx.AssocMap (member)
import PlutusLedgerApi.V3
    ( TxOutRef(..),
      ScriptContext(..),
      CurrencySymbol,
      TxInfo(..),
      TxInInfo (..),
      ScriptPurpose (..),
      TxOut (..),
      Value (..),
      PubKeyHash,
      TxCert (..), ToData (..), Datum (..), OutputDatum (..))
import PlutusTx
    ( compile,
      CompiledCode,
      makeIsDataIndexed )
import PlutusTx.Bool
    ( Bool(..),
      (&&), otherwise)
import PlutusTx.Numeric
    ( AdditiveGroup(..),
      AdditiveSemigroup (..) )

-- [General notes on this file]
-- This file contains two plutus scripts, the CC membership script and the locking script. 
-- The CC membership script is parameterized by the currency symbol of an NFT, and validates 
-- true if the NFT is in a transaction input. The locking script governs the unlocking of this NFT,
-- and therefore when a CC certificated can be witnessed via the CC membership script.

-- [CC membership script]
-- This script just checks that the hardcoded currency symbol of the NFT is in a transaction inputs.
{-# INLINABLE ccScript #-}
ccScript :: CurrencySymbol -> () -> ScriptContext -> Bool
ccScript symbol _ ctx = any (\value -> symbol `member` value) txInputsValues
    where
        -- the list of transaction inputs being consumed in this transaction.
        txInputs = txInfoInputs . scriptContextTxInfo $ ctx
        -- the list of value maps of the transaction inputs.
        txInputsValues = map (getValue . txOutValue . txInInfoResolved) txInputs

ccScriptCode :: CompiledCode (CurrencySymbol -> () -> ScriptContext -> Bool)
ccScriptCode = $$(compile [|| ccScript ||])

-- X509 is a data type that represents a commitment to an X509 certificate.
-- It is parameterized by the hash of the public key that is in the certificate.
-- To make the link between onchain and offchain bidirection the hash 
-- of the certificate is also in this type.
data X509 = X509 {
    pubKeyHash  :: PubKeyHash,
    x509Hash    :: BuiltinByteString
}
makeIsDataIndexed ''X509 [('X509, 0)]

instance Eq X509 where
    {-# INLINABLE (==) #-}
    (X509 pkh1 hash1) == (X509 pkh2 hash2) = pkh1 == pkh2 && hash1 == hash2

-- CCScriptDatum is the datum type of the locking script.
-- It is parameterized by two lists of X509 certificates, the cold and hot certificates.
-- The cold certificates have full control over the script address, while the hot certificates
-- can witness the unlocked utxo to issue a CC certificate (without any state transitions).
-- The hot certificates can also resign their power by removing themselves from the list.
-- Both list need an majority of signatures to do anything.
data CCScriptDatum = CCScriptDatum {
    coldX509s   :: [X509],
    hotX509s    :: [X509]
}
makeIsDataIndexed ''CCScriptDatum [('CCScriptDatum, 0)]

-- [Locking script actions]
-- The witness action is used to witness a transaction when a hot CC certificate needs to be issued.
-- It checks that there is only one input being spend from this script address, and that the value
-- plus datum of this input is replicated as an output. It also checks that 
data CCScriptRedeemer = Witness | Resign X509 | Unlock
makeIsDataIndexed ''CCScriptRedeemer [('Witness, 0), ('Resign, 1), ('Unlock, 2)]

-- [Locking script]
{-# INLINABLE lockingScript #-}
lockingScript :: CCScriptDatum -> CCScriptRedeemer -> ScriptContext -> Bool
lockingScript dtm red ctx = case scriptContextPurpose ctx of
    Spending txOurRef -> case red of
        Witness     -> checkTxOutPreservation && checkMultiSig (hotX509s dtm)
            where
                checkTxOutPreservation = case ownInput of
                    Just txOut  -> txOut `elem` txInfoOutputs txInfo
                    Nothing     -> False
        Resign x509 -> memberX509 && txSignedX509 && removeX509 && notWitnessed
            where
                memberX509 = x509 `elem` hotX509s'
                txSignedX509 = txSignedBy txInfo (pubKeyHash x509)
                hotX509s' = hotX509s dtm
                newDatum = CCScriptDatum (coldX509s dtm) (filter (/= x509) hotX509s')
                removeX509 = case ownInput of
                    Just txOut  -> let newTxOutput = txOut { txOutDatum = (OutputDatum . Datum . toBuiltinData) newDatum }
                                   in newTxOutput `elem` txInfoOutputs txInfo
                    Nothing     -> False
                notWitnessed = txInfoTxCerts txInfo == []
        Unlock      -> checkMultiSig (coldX509s dtm)
        where
            txInfo = scriptContextTxInfo ctx
            ownInput = txInInfoResolved <$> findTxInByTxOutRef txOurRef txInfo
            checkMultiSig list = majority <= numberOfSignatures
                where
                    majority = divide (length list) 2 + 1
                    numberOfSignatures = length $ filter (txSignedBy txInfo . pubKeyHash) list
    _                 -> False

lockingScriptCode :: CompiledCode (CCScriptDatum -> CCScriptRedeemer -> ScriptContext -> Bool)
lockingScriptCode = $$(compile [|| lockingScript ||])

-- remove the following when PlutusLedger.V3 exports these functions

-- | Check if a transaction is signed by the given public key hash.
{-# INLINABLE txSignedBy #-}
txSignedBy :: TxInfo -> PubKeyHash -> Bool
txSignedBy TxInfo{txInfoSignatories} k = case find (k ==) txInfoSignatories of
    Just _ -> True
    _      -> False

-- | Find the input that spends the given output reference.
{-# INLINABLE findTxInByTxOutRef #-}
findTxInByTxOutRef :: TxOutRef -> TxInfo -> Maybe TxInInfo
findTxInByTxOutRef outRef txInfo = find (\txIn -> txInInfoOutRef txIn == outRef) (txInfoInputs txInfo)

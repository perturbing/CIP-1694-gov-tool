{-# LANGUAGE TemplateHaskell                    #-}
{-# LANGUAGE NamedFieldPuns                     #-}
{-# LANGUAGE DataKinds                          #-}
{-# LANGUAGE NoImplicitPrelude                  #-}
{-# LANGUAGE ViewPatterns                       #-}
{-# LANGUAGE Strict                             #-}

{-# OPTIONS_GHC -fno-ignore-interface-pragmas   #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas     #-}
{-# OPTIONS_GHC -fno-full-laziness              #-}
{-# OPTIONS_GHC -fno-spec-constr                #-}
{-# OPTIONS_GHC -fno-specialise                 #-}
{-# OPTIONS_GHC -fno-strictness                 #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields        #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields  #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas       #-}
{-# HLINT ignore "Use null"                     #-}

module HotScripts where

import PlutusTx.Prelude
    ( (.),
      ($),
      length,
      even,
      divide,
      Ord (..),
      Eq (..), 
      Maybe (..), 
      maybe, 
      (<$>), 
      check,
      (/=), 
      modulo )
import PlutusTx.List
    ( any,
      map,
      elem,
      foldr,
      filter, 
      find )
import PlutusTx.Builtins
    ( BuiltinByteString, Integer, error, BuiltinData)
import PlutusTx.AssocMap (member, null)
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
      TxCert (..), 
      ToData (..), 
      FromData (..),
      Datum (..), 
      UnsafeFromData (..),
      OutputDatum (..))
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
import ColdScripts (ColdLockScriptDatum (..), X509 (..))

-- Helper function to wrap a script to error on the return of a False.
{-# INLINABLE wrapThreeArgs #-}
wrapThreeArgs :: ( UnsafeFromData a
             , UnsafeFromData b)
             => (a -> b -> ScriptContext -> Bool)
             -> (BuiltinData -> BuiltinData -> BuiltinData -> ())
wrapThreeArgs f a b ctx =
  check $ f
      (unsafeFromBuiltinData a)
      (unsafeFromBuiltinData b)
      (unsafeFromBuiltinData ctx)

{-# INLINABLE wrapFourArgs #-}
wrapFourArgs  :: (UnsafeFromData a
                , UnsafeFromData b
                , UnsafeFromData c)
                => (a -> b -> c -> ScriptContext -> Bool)
                -> (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
wrapFourArgs f a b c ctx =
  check $ f
      (unsafeFromBuiltinData a)
      (unsafeFromBuiltinData b)
      (unsafeFromBuiltinData c)
      (unsafeFromBuiltinData ctx)

-- [General notes on this file]
-- This file contains two plutus scripts, the first script will be used as the CC hot credential,
-- called 'hotCredentialScript'. The second script is a spending script called 'hotLockScript'.
-- The CC hot crendential script is parameterized by a currency symbol of a "Hot NFT", and evaluates
-- true if the NFT is in any spending input of the transaction. The hot lock script is parameterized
-- by the currency symbol of the NFT used in the cold credential setup. This "cold NFT" is used to
-- verifibably recover the list of X509 credentials used by the 'delegateX509s' group in the dutum
-- of the `coldLockScript` (via reference inputs).
--
-- The spending script governs the unlocking of the "Hot NFT", and thus how the hot credential can
-- be witnessed. This means that the main purpose of the hot lock script is vote on governance actions.
-- 
-- Note that the hot credential that is appointed by the the cold credential script can always override the
-- the function of this script. As appointing a different credential as a hot credential will make the old one 
-- obsolete.

-- [CC hot credential script]
-- This script just checks that the hard-coded currency symbol of the "Hot NFT" is 
-- in any spending input of the transaction.
{-# INLINABLE hotCredentialScript #-}
hotCredentialScript :: CurrencySymbol -> BuiltinData -> ScriptContext -> Bool
hotCredentialScript symbol _ ctx = case scriptContextPurpose ctx of
    Certifying _ _  -> any (\value -> symbol `member` value) txInputsValues
    _               -> False
    where
        -- The list of transaction inputs being consumed in this transaction.
        txInputs = txInfoInputs . scriptContextTxInfo $ ctx
        -- The list of value maps of the transaction inputs.
        txInputsValues = map (getValue . txOutValue . txInInfoResolved) txInputs

{-# INLINABLE mkWrappedHotCredentialScript #-}
mkWrappedHotCredentialScript :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedHotCredentialScript = wrapThreeArgs hotCredentialScript    

hotCredentialScriptCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
hotCredentialScriptCode = $$(compile [|| mkWrappedHotCredentialScript ||])

-- [Hot locking script actions]
-- This is the redeemer type of the locking script.
-- The vote action is used to witness the locked Hot NFT when a voting certificate needs to be witnessed.
-- The resign X509 action is used to resign a X509 CC certificate from the datum.
-- The recover action is for the `delegateX509s` certificates that are encoded in the datum of the cold locking script
-- to unlock the UTxO for any reason. This could for example be to rotate the certificates in the datum,
-- or unlock the Hot NFT to be able to lock it elsewhere (e.g. to a new updated hot locking script).
data HotLockScriptRedeemer = Vote | Resign X509 | Recover
makeIsDataIndexed ''HotLockScriptRedeemer [('Vote, 0), ('Resign, 1), ('Recover, 2)]

-- [Hot locking script]
-- The locking script takes in the redeemer types as above. The datum is a list of X509 certificates.
-- This script checks that for a given action in the redeemer (Vote, Resign X509, Recover) the
-- appropriate checks are made. These are as follows,
--
-- Vote: checks that the transaction input being spent is also an output, while preserving
-- the value and datum of this input (so there is no state transitions/movement of value).
-- Also, this action makes sure the transaction is signed by a majority of the certificates in the datum.
-- Effectively, this action witnesses control of the UTxO to be able to witness the hot CC credential,
-- and thus vote. Note that this action does not check that any vote is being done in the transaction!
--
-- Resign X509: checks that the provided X509 certificate from the redeemer is in the datum,
-- that the transaction is signed by this X509 certificate, and the certificate is removed from the datum.
-- Lastly, this action also checks that the transaction does not witness votes. To prevent 
-- unauthorized satisfaction of the above hot CC credential script.
--
-- Recover: checks that the transaction is signed by a majority of the delegate certificates.
-- this data is extracted from a reference input which contains the hard-coded currency symbol of the
-- "Cold NFT" used in the cold credential setup.
--
-- Note that this script requires the datum to always be an inlined datum. This so that 
-- off-chain tooling can parse the datum and verify X509 certificates against it.
{-# INLINABLE hotLockScript #-}
hotLockScript :: CurrencySymbol -> [X509] -> HotLockScriptRedeemer -> ScriptContext -> Bool
hotLockScript coldNFT dtm red ctx = case scriptContextPurpose ctx of
    Spending txOurRef -> case red of
        Vote     -> checkTxOutPreservation && checkMultiSig dtm
            where
                checkTxOutPreservation = case ownInput of
                    Just txOut  -> txOut `elem` txInfoOutputs txInfo
                    Nothing     -> False
        Resign x509 -> memberX509 && txSignedX509 && removedX509 && notWitnessed
            where
                memberX509 = x509 `elem` dtm
                txSignedX509 = txSignedBy txInfo (pubKeyHash x509)
                newDatum = filter (/= x509) dtm
                removedX509 = case ownInput of
                    Just txOut  -> let newTxOutput = txOut { txOutDatum = (OutputDatum . Datum . toBuiltinData) newDatum }
                                   in newTxOutput `elem` txInfoOutputs txInfo
                    Nothing     -> False
                notWitnessed = null (txInfoVotes txInfo)
        Recover      -> case refInput of
                    Just txOut  -> case txOutDatum txOut of
                        NoOutputDatum -> False
                        OutputDatumHash _ -> False
                        OutputDatum inlineDatum -> case (fromBuiltinData . getDatum) inlineDatum of
                            Just ccDtm -> checkMultiSig (recoveryX509s ccDtm)
                            Nothing -> False
                    Nothing     -> False
        where
            txInfo = scriptContextTxInfo ctx
            ownInput = txInInfoResolved <$> findTxInByTxOutRef txOurRef txInfo
            refInput = txInInfoResolved <$> findTxInByCurrencySymbolInRefUTxO coldNFT txInfo
            checkMultiSig list = majority <= numberOfSignatures
                where
                    majority = (\x -> divide x 2 + modulo x 2) $ length list
                    numberOfSignatures = length $ filter (txSignedBy txInfo . pubKeyHash) list
    _                 -> False

{-# INLINABLE wrappedHotLockScript #-}
wrappedHotLockScript :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedHotLockScript = wrapFourArgs hotLockScript

hotLockScriptCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
hotLockScriptCode = $$(compile [|| wrappedHotLockScript ||])

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

-- | Find the reference input that contains a certain currency sumbol.
{-# INLINABLE findTxInByCurrencySymbolInRefUTxO #-}
findTxInByCurrencySymbolInRefUTxO :: CurrencySymbol -> TxInfo -> Maybe TxInInfo
findTxInByCurrencySymbolInRefUTxO symbol txInfo = find (\txIn -> symbol `member` (getValue . txOutValue . txInInfoResolved ) txIn) (txInfoReferenceInputs txInfo)
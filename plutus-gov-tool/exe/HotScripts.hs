{-# LANGUAGE TemplateHaskell                    #-}
{-# LANGUAGE NamedFieldPuns                     #-}
{-# LANGUAGE DataKinds                          #-}
{-# LANGUAGE NoImplicitPrelude                  #-}
{-# LANGUAGE ViewPatterns                       #-}

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

-- this is actually the same code as in the cold scripts.
{-# INLINABLE votingCredentialScript #-}
votingCredentialScript :: CurrencySymbol -> BuiltinData -> ScriptContext -> Bool
votingCredentialScript symbol _ ctx = any (\value -> symbol `member` value) txInputsValues
    where
        -- The list of transaction inputs being consumed in this transaction.
        txInputs = txInfoInputs . scriptContextTxInfo $ ctx
        -- The list of value maps of the transaction inputs.
        txInputsValues = map (getValue . txOutValue . txInInfoResolved) txInputs

{-# INLINABLE mkWrappedVotingCredentialScript #-}
mkWrappedVotingCredentialScript :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVotingCredentialScript = wrapThreeArgs votingCredentialScript    

votingCredentialScriptCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
votingCredentialScriptCode = $$(compile [|| mkWrappedVotingCredentialScript ||])

data VotingScriptRedeemer = Vote | Resign X509 | Recover
makeIsDataIndexed ''VotingScriptRedeemer [('Vote, 0), ('Resign, 1), ('Recover, 2)]

{-# INLINABLE voteLockingScript #-}
voteLockingScript :: CurrencySymbol -> [X509] -> VotingScriptRedeemer -> ScriptContext -> Bool
voteLockingScript ccNFT dtm red ctx = case scriptContextPurpose ctx of
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
                notWitnessed = txInfoTxCerts txInfo == []
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
            refInput = txInInfoResolved <$> findTxInByCurrencySymbolInRefUTxO ccNFT txInfo
            checkMultiSig list = majority <= numberOfSignatures
                where
                    majority = (\x -> divide x 2 + modulo x 2) $ length list
                    numberOfSignatures = length $ filter (txSignedBy txInfo . pubKeyHash) list
    _                 -> False

{-# INLINABLE wrappedVoteLockingScript #-}
wrappedVoteLockingScript :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedVoteLockingScript = wrapFourArgs voteLockingScript

lockingVoteScriptCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
lockingVoteScriptCode = $$(compile [|| wrappedVoteLockingScript ||])

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
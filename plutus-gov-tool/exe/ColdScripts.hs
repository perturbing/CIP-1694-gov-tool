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

module ColdScripts where

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

{-# INLINABLE wrapTwoArgs #-}
wrapTwoArgs  :: (UnsafeFromData a)
                => (a -> ScriptContext -> Bool)
                -> (BuiltinData -> BuiltinData -> ())
wrapTwoArgs f a ctx =
  check $ f
      (unsafeFromBuiltinData a)
      (unsafeFromBuiltinData ctx)

-- [General notes on this file]
-- This file contains two plutus scripts, the script that will be used as the CC cold credential,
-- called 'coldCredentialScript'. The other script is a spending script called `coldLockScript`.
-- The CC cold credential script is parameterized by the currency symbol of a "Cold NFT", and evaluates 
-- true if the NFT is in any spending input of the transaction. 
-- The spending script governs the unlocking of this NFT, and therefore when and how
-- the cold CC credential witnesses a certificate that issues a hot voting credential.

-- [CC cold credential script]
-- This script just checks that the hard-coded currency symbol of the NFT is 
-- in any spending input of the transaction.
{-# INLINABLE coldCredentialScript #-}
coldCredentialScript :: CurrencySymbol -> BuiltinData -> ScriptContext -> Bool
coldCredentialScript symbol _ ctx =  case scriptContextPurpose ctx of
    Certifying _ _  -> any (\value -> symbol `member` value) txInputsValues
    _               -> False
    where
        -- The list of transaction inputs being consumed in this transaction.
        txInputs = txInfoInputs . scriptContextTxInfo $ ctx
        -- The list of value maps of the transaction inputs.
        txInputsValues = map (getValue . txOutValue . txInInfoResolved) txInputs

{-# INLINABLE mkWrappedColdCredentialScript #-}
mkWrappedColdCredentialScript :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedColdCredentialScript = wrapThreeArgs coldCredentialScript

coldCredentialScriptCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
coldCredentialScriptCode = $$(compile [|| mkWrappedColdCredentialScript ||])

-- X509 is a data type that represents a commitment to an X509 certificate.
-- It is given by the hash of the public key that is in the certificate.
-- To make the link between onchain and offchain two-way verifiable, the hash 
-- of the certificate is also in this type.
data X509 = X509 {
    pubKeyHash  :: PubKeyHash,
    x509Hash    :: BuiltinByteString
}
makeIsDataIndexed ''X509 [('X509, 0)]

instance Eq X509 where
    {-# INLINABLE (==) #-}
    (X509 pkh1 hash1) == (X509 pkh2 hash2) = pkh1 == pkh2 && hash1 == hash2

-- [Locking script datum]
-- ColdLockScriptDatum is the datum type of the CC NFT locking script.
-- It is given by a CA x509 certificate and two lists of X509 certificates, the recovery and delegete certificates.
-- The caX509s has no onchain function other than to be a commitment to the CA certificate.
-- The recovery certificates have full control over the script, while the delegate certificates
-- can only witness the UTxO to issue a hot CC crendential certificate (without any state transitions).
-- The delegate certificates can also resign their power by removing themselves from the list.
-- Both list need a majority of signatures to do anything.
data ColdLockScriptDatum = ColdLockScriptDatum {
    caX509         ::  X509,
    recoveryX509s   :: [X509],
    delegateX509s   :: [X509]
}
makeIsDataIndexed ''ColdLockScriptDatum [('ColdLockScriptDatum, 0)]

-- [Cold locking script actions]
-- This is the redeemer type of the locking script.
-- The delegate action is used to witness the locked UTxO when a hot CC credential needs to be issued.
-- The resign X509 action is used to resign a non recovery CC certificate.
-- The recover action is for the recover certificates to unlock the UTxO for any reason. This
-- could for example be to rotate the encoded recover certificates, remove a compromised certificate, or
-- unlock the NFT to be able to lock it elsewhere (e.g. to a new updated CC locking script).
data ColdLockScriptRedeemer = Delegate | Resign X509 | Recover
makeIsDataIndexed ''ColdLockScriptRedeemer [('Delegate, 0), ('Resign, 1), ('Recover, 2)]

-- [Cold locking script]
-- The locking script takes in the datum and redeemer types as above.
-- This script checks that for a given action in the redeemer (Delegate, Resign X509, Recover) the
-- appropriate checks are made. These are as follows,
--
-- Delegate: checks that the transaction input being spent is also an output, while preserving
-- the value and datum of this input (so there is no state transitions/movement of value).
-- Also, this action makes sure the transaction is signed by a majority of the delegate certificates.
-- Effectively, this action witnesses control of the UTxO to be able to issue a hot CC credential.
-- Note that this action does not check that this certificate is witnessed!
--
-- Resign X509: checks that the provided X509 certificate from the redeemer is in the delegate list,
-- that the transaction is signed by this X509 certificate, and the certificate is removed from the delegate list.
-- Lastly, this action also checks that the transaction does not witness any certificates. To prevent 
-- unauthorized satisfaction of the above cold CC credential script.
--
-- Recover: checks that the transaction is signed by a majority of the recovery certificates.
--
-- Note that this script requires the datum to always be an inlined datum. This so that 
-- off-chain tooling can parse the datum and verify X509 certificates against it.
{-# INLINABLE coldLockScript #-}
coldLockScript :: ColdLockScriptDatum -> ColdLockScriptRedeemer -> ScriptContext -> Bool
coldLockScript dtm red ctx = case scriptContextPurpose ctx of
    Spending txOurRef -> case red of
        Delegate     -> checkTxOutPreservation && checkMultiSig (delegateX509s dtm)
            where
                checkTxOutPreservation = case ownInput of
                    Just txOut  -> txOut `elem` txInfoOutputs txInfo
                    Nothing     -> False
        Resign x509 -> memberX509 && txSignedX509 && removedX509 && notWitnessed
            where
                memberX509 = x509 `elem` delegateX509s'
                txSignedX509 = txSignedBy txInfo (pubKeyHash x509)
                delegateX509s' = delegateX509s dtm
                newDatum = ColdLockScriptDatum (caX509 dtm) (recoveryX509s dtm) (filter (/= x509) delegateX509s')
                removedX509 = case ownInput of
                    Just txOut  -> let newTxOutput = txOut { txOutDatum = (OutputDatum . Datum . toBuiltinData) newDatum }
                                   in newTxOutput `elem` txInfoOutputs txInfo
                    Nothing     -> False
                notWitnessed = txInfoTxCerts txInfo == []
        Recover      -> checkMultiSig (recoveryX509s dtm)
        where
            txInfo = scriptContextTxInfo ctx
            ownInput = txInInfoResolved <$> findTxInByTxOutRef txOurRef txInfo
            checkMultiSig list = majority <= numberOfSignatures
                where
                    majority = (\x -> divide x 2 + modulo x 2) $ length list
                    numberOfSignatures = length $ filter (txSignedBy txInfo . pubKeyHash) list
    _                 -> False

{-# INLINABLE wrappedColdLockScript #-}
wrappedColdLockScript :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedColdLockScript = wrapThreeArgs coldLockScript

coldLockScriptCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
coldLockScriptCode = $$(compile [|| wrappedColdLockScript ||])

-- testing purposes

{-# INLINABLE alwaysTrueMint #-}
alwaysTrueMint :: BuiltinData -> ScriptContext -> Bool
alwaysTrueMint _ _ = True

{-# INLINABLE wrappedAlwaysTrueMint #-}
wrappedAlwaysTrueMint :: BuiltinData -> BuiltinData -> ()
wrappedAlwaysTrueMint = wrapTwoArgs alwaysTrueMint

alwaysTrueMintCode :: CompiledCode (BuiltinData -> BuiltinData -> ())
alwaysTrueMintCode = $$(compile [|| wrappedAlwaysTrueMint ||])

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
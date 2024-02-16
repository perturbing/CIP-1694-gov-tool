{-# LANGUAGE TemplateHaskell                    #-}
{-# LANGUAGE DataKinds                          #-}
{-# LANGUAGE NoImplicitPrelude                  #-}

{-# LANGUAGE Strict                             #-}

{-# OPTIONS_GHC -fno-ignore-interface-pragmas   #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas     #-}
{-# OPTIONS_GHC -fno-full-laziness              #-}
{-# OPTIONS_GHC -fno-spec-constr                #-}
{-# OPTIONS_GHC -fno-specialise                 #-}
{-# OPTIONS_GHC -fno-strictness                 #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields        #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields  #-}

module Scripts where

import PlutusTx.Prelude
    ( ($),
      check,
      bls12_381_G1_scalarMul,
      bls12_381_G1_uncompress,
      bls12_381_G1_compressed_generator,
      bls12_381_G1_add,
      bls12_381_G1_compressed_zero, 
      Eq (..) )
import PlutusTx.Builtins
    ( Integer, BuiltinData, )
import PlutusLedgerApi.V3
    ( ScriptContext(..), UnsafeFromData, unsafeFromBuiltinData )
import PlutusTx
    ( compile,
      CompiledCode)
import PlutusTx.Bool
    ( Bool(..))

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

{-# INLINABLE blsMint #-}
-- | A minting policy that returns true if n = 1
blsMint :: Integer -> ScriptContext -> Bool
blsMint n ctx = (==)
    (bls12_381_G1_scalarMul n g) -- g^n
    (bls12_381_G1_add g (bls12_381_G1_scalarMul 0 g)) -- g + g^0 = g^(1+0)=g^1
    where g = bls12_381_G1_uncompress bls12_381_G1_compressed_generator

{-# INLINABLE wrappedBlsMint #-}
wrappedBlsMint :: BuiltinData -> BuiltinData -> ()
wrappedBlsMint = wrapTwoArgs blsMint

blsMintCode :: CompiledCode (BuiltinData -> BuiltinData -> ())
blsMintCode = $$(compile [|| wrappedBlsMint ||])

{-# INLINABLE blsSpend #-}
-- | A spending policy that returns true if n = m+1
blsSpend :: Integer -> Integer -> ScriptContext -> Bool
blsSpend n m ctx = (==)
    (bls12_381_G1_scalarMul n g) -- g^n
    (bls12_381_G1_add g (bls12_381_G1_scalarMul m g)) -- g + g^m = g^(1+m)
    where g = bls12_381_G1_uncompress bls12_381_G1_compressed_generator

{-# INLINABLE wrappedBlsSpend #-}
wrappedBlsSpend :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedBlsSpend = wrapThreeArgs blsSpend

blsSpendCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
blsSpendCode = $$(compile [|| wrappedBlsSpend ||])
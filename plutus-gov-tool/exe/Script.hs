{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Script where

import PlutusTx.Prelude ( Bool(True), BuiltinData )
import PlutusTx.Builtins (BuiltinData)
import PlutusLedgerApi.V2.Contexts (TxOutRef (..), ScriptContext (..))
import PlutusTx (compile, CompiledCode)

{-# INLINABLE testMintScript #-}
testMintScript :: BuiltinData -> ScriptContext -> Bool
testMintScript _ _ = True

testScriptCode :: CompiledCode (BuiltinData -> ScriptContext -> Bool)
testScriptCode = $$(compile [|| testMintScript ||])
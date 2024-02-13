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

module ScriptsV2 where

import PlutusTx.Builtins
    ( BuiltinData )
import PlutusLedgerApi.V2
    ( ScriptContext(..))
import PlutusTx
    ( compile,
      CompiledCode,
      makeIsDataIndexed )
import PlutusTx.Bool
    ( Bool(..) )

alwaysTrueMint :: BuiltinData -> ScriptContext -> Bool
alwaysTrueMint _ _ = True

alwaysTrueMintCodeV2 :: CompiledCode (BuiltinData -> ScriptContext -> Bool)
alwaysTrueMintCodeV2 = $$(compile [|| alwaysTrueMint ||])

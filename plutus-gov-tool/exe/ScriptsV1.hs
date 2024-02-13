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

module ScriptsV1 where

import PlutusTx.Builtins
    ( BuiltinData )
import PlutusLedgerApi.V1
    ( ScriptContext(..))
import PlutusTx
    ( compile,
      CompiledCode,
      makeIsDataIndexed )
import PlutusTx.Bool
    ( Bool(..) )

alwaysTrueMint :: BuiltinData -> BuiltinData -> Bool
alwaysTrueMint _ _ = True

alwaysTrueMintCodeV1 :: CompiledCode (BuiltinData -> BuiltinData -> Bool)
alwaysTrueMintCodeV1 = $$(compile [|| alwaysTrueMint ||])

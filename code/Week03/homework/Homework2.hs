{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Homework2 where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext, Validator,TxInfo,txInfoValidRange,scriptContextTxInfo,
                                       mkValidatorScript)
import           PlutusTx             (applyCode, compile, liftCode)
import           PlutusTx.Prelude     (Bool , (.), traceIfFalse, (&&))
import           Utilities            (wrap)
import          Plutus.V2.Ledger.Contexts (txSignedBy)
import          Plutus.V1.Ledger.Interval (contains,from)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

{-# INLINABLE mkParameterizedVestingValidator #-}
-- This should validate if the transaction has a signature from the parameterized beneficiary and the deadline has passed.
mkParameterizedVestingValidator :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkParameterizedVestingValidator beneficiary deadline () ctx = traceIfFalse msg1 signedByBeneficiary &&
                                                              traceIfFalse msg2 deadLinepassed
    where 
        msg1 = "not signed by beneficiary"

        msg2 = "deadline not yet reached"

        info :: TxInfo
        info = scriptContextTxInfo ctx

        signedByBeneficiary :: Bool
        signedByBeneficiary = txSignedBy info beneficiary

        deadLinepassed :: Bool
        deadLinepassed = contains (from deadline) (txInfoValidRange info)


{-# INLINABLE  mkWrappedParameterizedVestingValidator #-}
mkWrappedParameterizedVestingValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedVestingValidator = wrap . mkParameterizedVestingValidator

validator :: PubKeyHash -> Validator
validator beneficiary = mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode beneficiary)

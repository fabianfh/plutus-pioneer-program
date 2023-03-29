{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext, Validator,TxInfo,txInfoValidRange,
                                       scriptContextTxInfo, mkValidatorScript)
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool (..),traceIfFalse,(&&),(||),(++),BuiltinString,($),not)
import           Utilities            (wrap)
-- import Data.Aeson (Value(String))
import          Plutus.V2.Ledger.Contexts (txSignedBy)
import          Plutus.V1.Ledger.Interval (contains,from,to)
import          PlutusTx.Builtins.Class (stringToBuiltinString,fromBuiltin)
import          PlutusTx.Builtins.Internal (ifThenElse,BuiltinBool (..))



---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator dat () ctx = traceIfFalse msg1 (signedByBeneficiary1 && beforedeadline) ||
                                traceIfFalse msg2 (signedByBeneficiary2 && not beforedeadline)
                        

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary1 :: Bool
    signedByBeneficiary1 =  txSignedBy info (beneficiary1 dat)

    signedByBeneficiary2 :: Bool
    signedByBeneficiary2 = txSignedBy info (beneficiary2 dat)

    beforedeadline :: Bool
    beforedeadline =  contains   (to (deadline dat)) (txInfoValidRange info)



    msg1 :: BuiltinString
    msg1 =  "Case1:"
                -- stringToBuiltinString $ 
                -- "Case1: signedByBeneficiary1: " ++
                -- ifThenElse (BuiltinBool signedByBeneficiary1) "True" "False" ++
                -- "; beforedeadline: " ++
                -- ifThenElse (BuiltinBool beforedeadline) "True"  "False" 

    msg2 :: BuiltinString
    msg2 =  "Case2:"
                -- stringToBuiltinString $ 
                -- "Case2: signedByBeneficiary2: " ++
                -- ifThenElse (BuiltinBool signedByBeneficiary2) "True"  "False" ++
                -- "; afterdeadline: " ++
                -- ifThenElse (BuiltinBool afterdeadline) "True"  "False" 


{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrap mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OnChain
  ( Order(..)
  , OrderAction(..)
  , adaxScript
  , adaxScriptShortBs
  ) where

import           Codec.Serialise
import           Plutus.V1.Ledger.Contexts
import qualified PlutusTx
import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.ByteString.Short    as SBS
import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified Plutus.V1.Ledger.Ada     as Native
import qualified Ledger.Typed.Scripts     as Scripts
import           Ledger.Value             (valueOf)
import           PlutusTx.Builtins
import           PlutusTx.Prelude         as P hiding (Semigroup (..), unless)
import           Ledger                   hiding (singleton)

data Order = Order
    { oOwner                     :: !PubKeyHash
    , baseAssetName              :: !TokenName
    , baseAssetSymbol            :: !CurrencySymbol
    , quoteAssetName             :: !TokenName
    , quoteAssetSymbol           :: !CurrencySymbol
    , price                      :: !Integer
    , priceDivisor               :: !Integer
    , ver                        :: !Integer
    }
PlutusTx.makeLift ''Order

data OrderAction = Take | Collect
PlutusTx.makeIsDataIndexed ''OrderAction [('Take, 0), ('Collect, 1)]
PlutusTx.makeLift ''OrderAction

{-# INLINABLE mkValidator #-}
mkValidator :: Order -> BuiltinData -> OrderAction -> ScriptContext -> Bool
mkValidator o _ act ctx =
    case act of
        Take ->
            areRecipientsCorrect &&
            case baseIsAda of
                  True ->
                      (amount <= qVal) &&
                      (adaFee <= feePaid)
                  False -> -- we assume it's ada
                      (addInteger amount (multiplyInteger adaFee 2) <= qVal)
        Collect ->
            (oOwner o == pHash) &&
            areRecipientsCorrect
  where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx

    sHash :: Plutus.ValidatorHash
    sHash = ownHash ctx

    validatorAddress :: Ledger.Address
    validatorAddress = scriptHashAddress sHash

    outputsTx :: [TxOut]
    outputsTx = txInfoOutputs txInfo

    pAddress :: Address
    pAddress = txOutAddress (head outputsTx)

    pHash :: PubKeyHash
    pHash = case (toPubKeyHash pAddress) of
      Just hash -> hash

    txOutAddresses :: [Address]
    txOutAddresses = map (\x -> txOutAddress x) outputsTx

    areRecipientsCorrect :: Bool
    areRecipientsCorrect = length (filter (\x -> (x /= validatorAddress) && (x /= pAddress)) txOutAddresses) == 0

    valueLocked :: Value
    valueLocked = valueLockedBy txInfo sHash

    qName :: TokenName
    qName = quoteAssetName o

    qSymbol :: CurrencySymbol
    qSymbol = quoteAssetSymbol o

    bName :: TokenName
    bName = baseAssetName o

    bSymbol :: CurrencySymbol
    bSymbol = baseAssetSymbol o

    scriptInputs :: [TxInInfo]
    scriptInputs = filter (\x -> txOutAddress (txInInfoResolved x) == validatorAddress) (txInfoInputs txInfo)

    adaFee :: Integer
    adaFee = multiplyInteger (length scriptInputs) 1689618

    baseIsAda :: Bool
    baseIsAda = bName == Native.adaToken && bSymbol == Native.adaSymbol

    feePaid :: Integer
    feePaid = valueOf valueLocked Native.adaSymbol Native.adaToken

    baseAmounts :: [Integer]
    baseAmounts = map (\x -> valueOf (txOutValue (txInInfoResolved x)) bSymbol bName) scriptInputs

    bVal :: Integer
    bVal = sum baseAmounts

    qVal :: Integer
    qVal = valueOf valueLocked qSymbol qName

    amount :: Integer
    amount = divideInteger (multiplyInteger bVal (price o)) (priceDivisor o)

data Adax
instance Scripts.ValidatorTypes Adax where
    type instance DatumType    Adax = BuiltinData
    type instance RedeemerType Adax = OrderAction

typedValidator :: Order -> Scripts.TypedValidator Adax
typedValidator o = Scripts.mkTypedValidator @Adax
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode o)
    $$(PlutusTx.compile  [|| wrap        ||])
  where
    wrap = Scripts.wrapValidator @BuiltinData @OrderAction

adaxValidator :: Order -> Validator
adaxValidator = Scripts.validatorScript . typedValidator

adaxPlutusScript :: Order -> Plutus.Script
adaxPlutusScript = Plutus.unValidatorScript . adaxValidator

adaxScriptShortBs :: Order -> SBS.ShortByteString
adaxScriptShortBs = SBS.toShort . LBS.toStrict . serialise . adaxPlutusScript

adaxScript :: Order -> PlutusScript PlutusScriptV1
adaxScript = PlutusScriptSerialised . adaxScriptShortBs

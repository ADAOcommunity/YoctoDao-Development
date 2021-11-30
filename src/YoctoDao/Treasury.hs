{--
   Copyright 2021 ₳DAO

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
--}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE LambdaCase          #-}

module YoctoDao.Treasury where

import           Prelude                (String, show, Show)
import           Control.Monad          hiding (fmap)
import           PlutusTx.Maybe
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract
import qualified PlutusTx
import           PlutusTx.IsData
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (singleton)
import           Ledger.Ada             as Ada
import           Ledger.Constraints     as Constraints
import           Ledger.Index           as Index
import qualified Ledger.Typed.Scripts   as Scripts
import qualified Ledger.Contexts                   as Validation
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema, NonEmpty(..) )
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions, ensureKnownCurrencies)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (Semigroup (..))
import           Text.Printf            (printf)
import           GHC.Generics         (Generic)
import           Data.String          (IsString (..))
import           Data.Aeson           (ToJSON, FromJSON)
import           Playground.Contract

{-# INLINABLE treasuryValidator #-}
treasuryValidator :: AssetClass -> BuiltinData -> BuiltinData -> ScriptContext -> Bool
treasuryValidator asset _ _ ctx =
  let
      txInfo = scriptContextTxInfo ctx

      -- We map over all of the inputs to the transaction to gather the number of votes present.
      txInValues = [txOutValue $ txInInfoResolved txIn | txIn <- txInfoInputs $ scriptContextTxInfo ctx]
      tokenValues = [assetClassValueOf val asset | val <- txInValues]
      votes = sum tokenValues -- sum the occurrences of the tokenClass inside of txInValues
  in
      traceIfFalse "The DAO's NFT is not present." (votes > 0)

data TreasuryData
instance Scripts.ValidatorTypes TreasuryData where
    type instance DatumType TreasuryData = BuiltinData
    type instance RedeemerType TreasuryData = BuiltinData

treasuryValidatorInstance :: AssetClass -> Scripts.TypedValidator TreasuryData
treasuryValidatorInstance asset = Scripts.mkTypedValidator @TreasuryData
    ($$(PlutusTx.compile [|| treasuryValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode asset)
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @BuiltinData @BuiltinData

treasuryValidatorHash :: AssetClass -> ValidatorHash
treasuryValidatorHash = Scripts.validatorHash . treasuryValidatorInstance

treasuryValidatorScript :: AssetClass -> Validator
treasuryValidatorScript = Scripts.validatorScript . treasuryValidatorInstance

treasuryValidatorAddress :: AssetClass -> Address
treasuryValidatorAddress = Ledger.scriptAddress . treasuryValidatorScript --}
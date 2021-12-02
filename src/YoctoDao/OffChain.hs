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

module YoctoDao.OffChain where

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
import           Ledger.Address         as Address
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

import           YoctoDao.Core
import           YoctoDao.GovToken
import           YoctoDao.Treasury

{-- We need to create the proposal using the IdMaker NFT so that it will be valid, also ensure a valid proposal..
data CreateProposal = CreateProposal
    { proposalStart :: !POSIXTime
    , scriptUpdate  :: !Bool
    , scriptAddr    :: Address
    , spend         :: !Bool
    , spendValue    :: Value
    , spendAddr     :: Address
    , minting       :: !Bool
    , minted        :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

createProposal :: forall w s. CreateProposal -> Contract w s Text ()
createProposal cp = do
    scriptPubKeyHash <- pubKeyHash <$> Contract.ownPubKey
    let proposal = Proposal
            { proposalStart = proposalStart cp
            , scriptUpdate  = scriptUpdate cp
            , scriptAddr = scriptAddr cp
            , spend = spend cp
            , spendValue = spendValue cp
            , spendAddr = spendAddr cp
            , minting = minting cp
            , minted = minted cp
            , yes = 0
            , no = 0
            } --}
        
createOwnerId :: forall w s. ValidatorHash -> AssetClass -> AssetClass -> AssetClass -> AssetClass -> AssetClass -> Integer -> Contract w s Text ()
createOwnerId treasury govClass nft idMaker propclass voteclass co = do
    let scriptAddr = votingPassScriptAddress treasury govClass nft idMaker propclass voteclass
        treasuryAddr = Address.scriptHashAddress treasury
    pkh <- Contract.ownPubKeyHash
    now <- currentTime
    utxos <- utxosAt scriptAddr
    futxos <- utxosAt (Address.pubKeyHashAddress pkh)

    let utxos' = Map.filter (\v -> 1 == assetClassValueOf (_ciTxOutValue v) idMaker) utxos
        futxos' = Map.filter (\v -> 1 <= assetClassValueOf (_ciTxOutValue v) govClass) futxos
        (nftRef,nftTx) = head $ Map.toList utxos'
        valueToScript = assetClassValue voteclass 1 <> assetClassValue govClass co
        ownership = Ownership
            { owner = pkh
            , nftSlot = now
            , lastTransfer = now
            }
        oDatum = Datum $ toBuiltinData ownership

        validator = votingPassValidatorScript treasury govClass nft idMaker propclass voteclass
        validatorHash = votingPassValidatorHash treasury govClass nft idMaker propclass voteclass

        lookups = Constraints.ownPubKeyHash pkh <>
                  Constraints.mintingPolicy (policy idMaker) <>
                  Constraints.otherScript validator <>
                  Constraints.unspentOutputs utxos' <>
                  Constraints.unspentOutputs futxos'

        tx = Constraints.mustSpendScriptOutput nftRef (Redeemer $ toBuiltinData ()) <>
             Constraints.mustMintValue (assetClassValue voteclass 1) <>
             Constraints.mustPayToOtherScript validatorHash oDatum valueToScript

    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "Created Ownership: %s" (show ownership)

data SpendOwned = SpendOwned
    { spendAddr  :: !Address
    , spendVal   :: !Value
    , spendDatum :: Datum
    } deriving (Show, Generic, FromJSON, ToJSON)

spendOwnershipUTxO :: forall w s. ValidatorHash -> AssetClass -> AssetClass -> AssetClass -> AssetClass -> AssetClass -> SpendOwned -> Contract w s Text ()
spendOwnershipUTxO treasury govClass nft idMaker propclass voteclass spend = do
    Contract.logInfo @String $ printf "Spent owned UTxO to: %s" (show spend)

-- findIdMaker :: forall w s. Address -> AssetClass -> Contract w s Text ()

-- We need to create a transaction that if going to the script will contain the Id for the script,
--data ShiftOwnership = ShiftOwnership
-- We need to apply owned votes with the proposal and send all votes back to the script with the ownership FT attached.
--data ApplyVotes = ApplyVotes
-- We need to check to see what type of proposal is being executed, this should be done through an input of a proposal datum..?
--data ExecuteProposal = ExecuteProposal
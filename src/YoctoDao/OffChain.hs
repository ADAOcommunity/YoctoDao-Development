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
        
-- We need to create the Ownership Datum UTxO with the IdMaker NFT so that it will be valid for use in proposals.
data CreateOwnerId = CreateOwnerId
    { createOwner :: !PubKeyHash
    } deriving (Show, Generic, FromJSON, ToJSON)

createOwnerId :: forall w s. CreateOwnerId -> Contract w s Text ()
createOwnerId co = do
    scriptPubKeyHash <- Contract.ownPubKeyHash
    now <- currentTime
    -- utxos <- Map.filter idIdMaker <$> utxoAt
    let ownership = Ownership
            { owner = createOwner co
            , nftSlot = now
            , lastTransfer = now
            }
    Contract.logInfo @String $ printf "Created Ownership: %s" (show ownership)
  where
    isIdMaker :: TxOutTx -> Bool
    isIdMaker o = True

-- We need to create a transaction that if going to the script will contain the Id for the script,
--data ShiftOwnership = ShiftOwnership
-- We need to apply owned votes with the proposal and send all votes back to the script with the ownership FT attached.
--data ApplyVotes = ApplyVotes
-- We need to check to see what type of proposal is being executed, this should be done through an input of a proposal datum..?
--data ExecuteProposal = ExecuteProposal
{--
    WARNING :: DO NOT USE
    This is untested and unfinished, the other files are useful but this one is not.

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

data DaoParams = DaoParams
    { daoTreasure  :: !ValidatorHash
    , daoGovToken  :: !AssetClass
    , daoIdNFT     :: !AssetClass
    , daoIdMaker   :: !AssetClass
    , daoPropClass :: !AssetClass
    , daoOwnClass  :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON)

data SpendOwned = SpendOwned
    { spendHash :: !PubKeyHash
    , spendVal  :: !Value
    } deriving (Show, Generic, FromJSON, ToJSON)

instance ToSchema SpendOwned

data CreateProposal = CreateProposal
    { proposalStart :: !POSIXTime
    , scriptUpdate  :: !Bool
    , scriptAddr    :: ValidatorHash
    , spend         :: !Bool
    , spendValue    :: Value
    , spendAddr     :: ValidatorHash
    , minting       :: !Bool
    , minted        :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

--PlutusTx.makeLift ''CreateProposal
instance ToSchema CreateProposal

data VoteInput = VoteInput
    { proposal :: !CreateProposal
    , yes      :: !Bool
    } deriving (Show, Generic, FromJSON, ToJSON)

--PlutusTx.makeLift ''VoteInput
instance ToSchema VoteInput

type YoctoSchema = Endpoint "0-InitializeDao" () .\/
                   Endpoint "1-CreateOwnership" Integer .\/
                   Endpoint "2-SpendOwnedTx" SpendOwned .\/
                   Endpoint "3-CreateProposal" CreateProposal .\/
                   Endpoint "4-VoteProposal" VoteInput .\/
                   Endpoint "5-SpendIdNft" CreateProposal

aDatum :: GovernanceDatum
aDatum = GIdMaker ()

startDao :: forall w s. DaoParams -> Contract w s Text YDao
startDao op = do
    let yDao = YDao
            { yTreasury = daoTreasure op
            , yGovClass = daoGovToken op
            , yNft = daoIdNFT op
            , yIdMaker = daoIdMaker op
            , yPropClass = daoPropClass op
            , yOwnClass = daoOwnClass op
            }
    logInfo @String $ "started dao" ++ show yDao
    return yDao

useDao :: forall w s. YDao -> () -> Contract w s Text ()
useDao yDao usage = do
    m <- findDaoIdMaker yDao
    let c = Constraints.mustPayToTheScript (GIdMaker $ usage) $ (assetClassValue (yNft yDao) 1 <> assetClassValue (yIdMaker yDao) 1)
    case m of
        Nothing -> do
            ledgerTx <- submitTxConstraints (votingPassValidatorInstance yDao) c
            awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "Dao instantiated."
        Just (oref, o,  _) -> do
            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>
                          Constraints.typedValidatorLookups (votingPassValidatorInstance yDao) <>
                          Constraints.otherScript (votingPassValidatorScript yDao)
                tx      = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ())
            -- ledgerTx <- submitTxConstraintsWith @Voting lookups tx
            -- awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "This is not supposed to occur. You likely initialized already."

findDaoNFT :: forall w s. YDao -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, Integer))
findDaoNFT dao = do
    utxos <- Map.filter f <$> utxosAt (votingPassScriptAddress dao)
    return $ case Map.toList utxos of
        [(oref, o)] -> do
            return (oref, o, 42)
        _           -> Nothing
  where
    f :: ChainIndexTxOut -> Bool
    f o = assetClassValueOf (_ciTxOutValue o) (yNft dao) == 1

findDaoIdMaker :: forall w s. YDao -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, ()))
findDaoIdMaker dao = do
    utxos <- Map.filter f <$> (utxosAt (votingPassScriptAddress dao))
    return $ case Map.toList utxos of
        [(oref, o)] -> do
            return (oref, o, ())
        _           -> Nothing
  where
    f :: ChainIndexTxOut -> Bool
    f o = assetClassValueOf (_ciTxOutValue o) (yIdMaker dao) == 1

findProposal :: forall w s. YDao -> CreateProposal -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, ()))
findProposal dao proposal = do
    utxos <- Map.filter f <$> (utxosAt (votingPassScriptAddress dao))
    return $ case Map.toList utxos of
        [(oref, o)] -> do
            return (oref, o, ())
        (oref, o):os -> do
            return (oref, o, ())
        _          -> Nothing
  where
    f :: ChainIndexTxOut -> Bool
    f o = assetClassValueOf (_ciTxOutValue o) (yPropClass dao) >= 1 &&
          True
          -- Replace true with the datum analysis

createOwnerId :: forall w s. YDao -> Integer -> Contract w s Text ()
createOwnerId yDao co = do
    Contract.logInfo @String $ printf "Creating Ownership..."
    let scriptAddr = votingPassScriptAddress yDao
        treasuryAddr = Address.scriptHashAddress (yTreasury yDao)
    m <- findDaoIdMaker yDao
    pkh <- Contract.ownPubKeyHash
    now <- currentTime
    utxos <- utxosAt scriptAddr
    futxos <- utxosAt (Address.pubKeyHashAddress pkh)
    let c = Constraints.mustPayToTheScript (GIdMaker $ ()) (assetClassValue (yIdMaker yDao) 1) <>
            Constraints.mustPayToTheScript (GIdMaker $ ()) (assetClassValue (yNft yDao) 1)

    case m of
        Nothing -> do
            ledgerTx <- submitTxConstraints (votingPassValidatorInstance yDao) c
            awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "Initialized DAO again?"
        Just (oref, o, _) -> do
            let utxos' = Map.filter (\v -> 1 == assetClassValueOf (_ciTxOutValue v) (yIdMaker yDao)) utxos
                futxos' = Map.filter (\v -> 1 <= assetClassValueOf (_ciTxOutValue v) (yGovClass yDao)) futxos
                (nftRef,nftTx) = head $ Map.toList utxos'
                valueToScript = assetClassValue (yOwnClass yDao) 1 <> assetClassValue (yGovClass yDao) co
                ownership = Ownership
                    { owner = pkh
                    , nftSlot = now
                    , lastTransfer = now
                    }
                oDatum = GOwnership ownership

                validator = votingPassValidatorScript yDao
                validatorI = votingPassValidatorInstance yDao
                validatorHash = votingPassValidatorHash yDao

                lookups = Constraints.ownPubKeyHash pkh <>
                        Constraints.mintingPolicy (policy (yIdMaker yDao)) <>
                        Constraints.otherScript validator <>
                        Constraints.unspentOutputs utxos' <>
                        Constraints.unspentOutputs futxos'

                tx = Constraints.mustSpendScriptOutput nftRef unitRedeemer <>
                    Constraints.mustPayToTheScript aDatum (_ciTxOutValue nftTx) <>
                    Constraints.mustMintValue (assetClassValue (yOwnClass yDao) 1) <>
                    Constraints.mustPayToTheScript oDatum valueToScript

            ledgerTx <- submitTxConstraintsWith @Voting lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "Created Ownership: %s" (show ownership)

spendOwnershipUTxOToWallet :: forall w s. YDao -> SpendOwned -> Contract w s Text ()
spendOwnershipUTxOToWallet yDao spendToWallet = do -- TODO I don't think that this is adequate.
    let scriptAddr = votingPassScriptAddress yDao
        treasuryAddr = Address.scriptHashAddress (yTreasury yDao)
    pkh <- Contract.ownPubKeyHash
    now <- currentTime
    utxos <- utxosAt scriptAddr
    futxos <- utxosAt (Address.pubKeyHashAddress pkh)

    let valueSpentByOwner = spendVal spendToWallet
        validator = votingPassValidatorScript yDao
        validatorHash = votingPassValidatorHash yDao

        lookups = Constraints.otherScript validator
                  -- Constraints.ownPubKeyHash pkh <>
                  -- Constraints.mintingPolicy (policy idMaker) <>

        --tx = Constraints.mustSpendScriptOutput nftRef (Redeemer $ toBuiltinData ()) <>
        tx = Constraints.mustPayToPubKey (spendHash spendToWallet) valueSpentByOwner

    ledgerTx <- submitTxConstraintsWith @Voting lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "Spent owned UTxO to: %s" (show spendToWallet)

createProposal :: forall w s. YDao -> CreateProposal -> Contract w s Text ()
createProposal yDao pInput = do
    Contract.logInfo @String $ printf "Creating Proposal"
    let scriptAddr = votingPassScriptAddress yDao
        treasuryAddr = Address.scriptHashAddress (yTreasury yDao)
    m <- findDaoIdMaker yDao
    pkh <- Contract.ownPubKeyHash
    now <- currentTime
    utxos <- utxosAt scriptAddr
    futxos <- utxosAt (Address.pubKeyHashAddress pkh)
    let c = Constraints.mustPayToTheScript (GIdMaker $ ()) (assetClassValue (yIdMaker yDao) 1) <>
            Constraints.mustPayToTheScript (GIdMaker $ ()) (assetClassValue (yNft yDao) 1)

    case m of
        Nothing -> do
            ledgerTx <- submitTxConstraints (votingPassValidatorInstance yDao) c
            awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "Initialized DAO again?"
        Just (oref, o, _) -> do
            return () -- TODO this endpoint needs to be implemented.
            -- If we have the IdMaker then we want to mint a proposal token and validate the proposal values, then create the UTxO with the Datum and the proposal token.

voteProposal :: forall w s. YDao -> VoteInput -> Contract w s Text ()
voteProposal yDao vInput = do
    Contract.logInfo @String $ printf "Vote Proposal"
    let scriptAddr = votingPassScriptAddress yDao
        treasuryAddr = Address.scriptHashAddress (yTreasury yDao)
    m <- findProposal yDao (proposal vInput)

    let c = Constraints.mustPayToTheScript (GIdMaker $ ()) (assetClassValue (yIdMaker yDao) 1) <>
            Constraints.mustPayToTheScript (GIdMaker $ ()) (assetClassValue (yNft yDao) 1)

    case m of
        Nothing -> do
            ledgerTx <- submitTxConstraints (votingPassValidatorInstance yDao) c
            awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "Initialized DAO again?"
        Just (oref, o, _) ->
            return () -- TODO This endpoint needs to be implemented.
            -- 
-- Need to implement
--  - gatherOwnGov

useProposal :: forall w s. YDao -> CreateProposal -> Contract w s Text ()
useProposal yDao proposal = do return ()
-- Helper functions for this: TODO this is not implemented
--  - 
--  - 

{-- runDao :: DaoParams -> Contract (Last YDao) YoctoSchema Text ()
runDao op = do
    dao <- startDao op
    tell $ Last $ Just dao
    go dao
  where
    go :: YDao -> Contract (Last YDao) YoctoSchema Text a
    go yDao = do
        x <- endpoint @"update"
        --updateOracle oracle x
        go oracle --}

endpoints :: YDao -> Contract () YoctoSchema Text ()
endpoints yDao = forever
                $ handleError logError
                $ awaitPromise
                $ initialize' `select`
                  createOwnerId' `select`
                  spendOwnershipUTxO' `select`
                  createProposal' `select`
                  voteProposal' `select`
                  useProposal'
  where
    initialize' = endpoint @"0-InitializeDao" $ useDao yDao
    createOwnerId' = endpoint @"1-CreateOwnership" $ createOwnerId yDao
    spendOwnershipUTxO' = endpoint @"2-SpendOwnedTx" $ spendOwnershipUTxOToWallet yDao
    createProposal' = endpoint @"3-CreateProposal" $ createProposal yDao
    voteProposal' = endpoint @"4-VoteProposal" $ voteProposal yDao
    useProposal' = endpoint @"5-SpendIdNft" $ useProposal yDao
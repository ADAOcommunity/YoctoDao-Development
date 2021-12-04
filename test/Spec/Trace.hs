{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Spec.Trace (
    tests
    , successfulOwnershipLockup
    ) where

import           Control.Monad              hiding (fmap)
import qualified Ledger
import           Ledger.Ada
import           Ledger.Value               as Value
import           Plutus.Contract.Test
import qualified Plutus.Trace.Emulator      as Trace
import           Test.Tasty

import qualified YoctoDao.Treasury          as Treasury
import qualified YoctoDao.GovToken          as GovToken
import qualified YoctoDao.Core              as Core
import qualified YoctoDao.OffChain          as OffChain

-- Contracts' parameters

-- for EmulatorTrace it must be a 28-byte length ByteString
nftSymbol :: Ledger.CurrencySymbol
nftSymbol = currencySymbol "0123456789012345678901234567"

nftName :: Ledger.TokenName
nftName = TokenName "ADAO"

idMakerName :: Ledger.TokenName
idMakerName = TokenName "IdMaker"

adaoNFT :: Ledger.AssetClass
adaoNFT = Value.assetClass nftSymbol nftName

idMakerClass :: Ledger.AssetClass
idMakerClass = Value.assetClass nftSymbol idMakerName

govTokenName :: Ledger.TokenName
govTokenName  = TokenName "GovToken"

ownedTokenName :: Ledger.TokenName
ownedTokenName  = TokenName "Owned"

proposalTokenName :: Ledger.TokenName
proposalTokenName = TokenName "Proposal"

govTokenClass :: Ledger.AssetClass
govTokenClass = Value.assetClass (GovToken.curSymbol adaoNFT) govTokenName

ownedTokenClass :: Ledger.AssetClass
ownedTokenClass = Value.assetClass (GovToken.curSymbol idMakerClass) ownedTokenName

proposalTokenClass :: Ledger.AssetClass
proposalTokenClass = Value.assetClass (GovToken.curSymbol idMakerClass) proposalTokenName

treasuryHash :: Ledger.ValidatorHash
treasuryHash = Treasury.treasuryValidatorHash adaoNFT

tests :: TestTree
tests = 
    let
        contract = OffChain.endpoints treasuryHash govTokenClass adaoNFT idMakerClass proposalTokenClass ownedTokenClass
    in
        testGroup "YoctoDaoTrace"
        [
        checkPredicate "Lockup Ownership Tokens Available" (endpointAvailable @"1-CreateOwnership" contract (Trace.walletInstanceTag w1)) $ void (Trace.activateContractWallet w1 contract),
        checkPredicate "Successful at Locking Tokens" assertNoFailedTransactions successfulOwnershipLockup
        ]

successfulOwnershipLockup :: Trace.EmulatorTrace ()
successfulOwnershipLockup = do
    h1 <- Trace.activateContractWallet (knownWallet 1) (OffChain.endpoints treasuryHash govTokenClass adaoNFT idMakerClass proposalTokenClass ownedTokenClass)
    -- h2 <- Trace.activateContractWallet (knownWallet 2) Fracada.endpoints

    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"1-CreateOwnership" h1 800
    {-- OffChain.SpendOwned { OffChain.spendAddr = Ledger.scriptHashAddress treasuryHash
                                                                , OffChain.spendVal = Ledger.Ada.lovelaceValueOf 0
                                                                , OffChain.spendDatum = 100 -- toBuiltinData ()
                                                                }              -- OffChain.ToFraction { Fracada.nftAsset = nftAssetClass
                                                                -- , Fracada.fractions = fractions
                                                                -- , Fracada.fractionTokenName = fractionTokenName 
                                                                --}
    void $ Trace.waitNSlots 1

    -- Trace.callEndpoint @"2-returnNFT" h2 nftAssetClass
    -- void $ Trace.waitNSlots 1

successfulOwnershipSpent :: Trace.EmulatorTrace ()
successfulOwnershipSpent = do
    h1 <- Trace.activateContractWallet (knownWallet 1) (OffChain.endpoints treasuryHash govTokenClass adaoNFT idMakerClass proposalTokenClass ownedTokenClass)
    -- h2 <- Trace.activateContractWallet (knownWallet 2) Fracada.endpoints

    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"2-SpendOwnedTx" h1 OffChain.SpendOwned { OffChain.spendAddr = Ledger.scriptHashAddress treasuryHash
                                                                , OffChain.spendVal = Ledger.Ada.lovelaceValueOf 0
                                                                , OffChain.spendDatum = 100 -- toBuiltinData ()
                                                                }

    void $ Trace.waitNSlots 1

    -- Trace.callEndpoint @"2-returnNFT" h2 nftAssetClass
    -- void $ Trace.waitNSlots 1
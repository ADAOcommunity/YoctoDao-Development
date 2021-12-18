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
import qualified PlutusTx
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

daoInput = Core.YDao
            { Core.yTreasury = treasuryHash
            , Core.yGovClass = govTokenClass
            , Core.yNft = adaoNFT
            , Core.yIdMaker = idMakerClass
            , Core.yPropClass = proposalTokenClass
            , Core.yOwnClass = ownedTokenClass
            }

{-- econf :: EmulatorConfig
econf = EmulatorConfig {_initialChainState = Left (fromList [
    (Wallet 1,Value (Map [
        (,Map [("",100000000)]),
        (nftSymbol,Map [("ADAO",1),("IdMaker",1)]),
        ((GovToken.curSymbol adaoNFT),Map [("GovToken",1000)])
        ])),
    (Wallet 2,Value (Map [(,Map [("",100000000)])])),
    (Wallet 3,Value (Map [(,Map [("",100000000)])])),
    (Wallet 4,Value (Map [(,Map [("",100000000)])])),
    (Wallet 5,Value (Map [(,Map [("",100000000)])])),
    (Wallet 6,Value (Map [(,Map [("",100000000)])])),
    (Wallet 7,Value (Map [(,Map [("",100000000)])])),
    (Wallet 8,Value (Map [(,Map [("",100000000)])])),
    (Wallet 9,Value (Map [(,Map [("",100000000)])])),
    (Wallet 10,Value (Map [(,Map [("",100000000)])]))])} --}

tests :: TestTree
tests = 
    let
        contract = OffChain.endpoints daoInput
    in
        testGroup "YoctoDaoTrace"
        [
        checkPredicate "Lockup Ownership Tokens Available" (endpointAvailable @"1-CreateOwnership" contract (Trace.walletInstanceTag w1)) $ void (Trace.activateContractWallet w1 contract),
        checkPredicate "Successful Initialization" assertNoFailedTransactions successfulInitialization,
        checkPredicate "Successful at Locking Tokens" assertNoFailedTransactions successfulOwnershipLockup,
        checkPredicate "Successful lock and spend to self." assertNoFailedTransactions successfulOwnershipSpent
        ]

successfulInitialization :: Trace.EmulatorTrace ()
successfulInitialization = do
    h1 <- Trace.activateContractWallet (knownWallet 1) (OffChain.endpoints daoInput)
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"0-InitializeDao" h1 ()
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"0-InitializeDao" h1 ()
    void $ Trace.waitNSlots 1

successfulOwnershipLockup :: Trace.EmulatorTrace ()
successfulOwnershipLockup = do
    h1 <- Trace.activateContractWallet (knownWallet 1) (OffChain.endpoints daoInput)
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"0-InitializeDao" h1 ()
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"1-CreateOwnership" h1 800
    void $ Trace.waitNSlots 1

successfulOwnershipSpent :: Trace.EmulatorTrace ()
successfulOwnershipSpent = do
    h1 <- Trace.activateContractWallet (knownWallet 1) (OffChain.endpoints daoInput)
    h2 <- Trace.activateContractWallet (knownWallet 2) (OffChain.endpoints daoInput)

    void $ Trace.waitNSlots 1

    -- Trace.callEndpoint @"1-CreateOwnership" h1 800
    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"2-SpendOwnedTx" h1 OffChain.SpendOwned { OffChain.spendHash = walletPubKeyHash (knownWallet 1)
                                                                , OffChain.spendVal = assetClassValue govTokenClass 800
                                                                }
    void $ Trace.waitNSlots 1

sanityTesting :: Trace.EmulatorTrace ()
sanityTesting = do -- If this is succeeding we are in the wrong, we need to ensure that we aren't paying from the pubkey right?
    h1 <- Trace.activateContractWallet (knownWallet 1) (OffChain.endpoints daoInput)
    h2 <- Trace.activateContractWallet (knownWallet 2) (OffChain.endpoints daoInput)

    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"1-CreateOwnership" h1 800
    void $ Trace.waitNSlots 1

    Trace.callEndpoint @"2-SpendOwnedTx" h2 OffChain.SpendOwned { OffChain.spendHash = walletPubKeyHash (knownWallet 1)
                                                                , OffChain.spendVal = assetClassValue govTokenClass 800
                                                                }
    void $ Trace.waitNSlots 1
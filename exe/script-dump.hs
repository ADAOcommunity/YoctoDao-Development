
{-# LANGUAGE OverloadedStrings   #-}
import           Prelude
import           System.Environment

import           Cardano.Api
import           Cardano.Api.Shelley
import           Codec.Serialise

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api as Plutus

import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LB

import           Ledger                     (datumHash, scriptHashAddress)


import           YoctoDao.GovToken
import           YoctoDao.Treasury
import           YoctoDao.Core

import          Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Api
import Data.String                         (IsString (..))
import           Data.Aeson
import GHC.Num (encodeDoubleInteger)

nftSymbol = fromString "2f1ca92cbb53b23e214393fa230a25da97bbcc9e55d76e211a4deccf" -- MAINNET
-- nftSymbol = fromString "2ab7e0898d0059a7859ed219e7d0e0bfe15e148cd07efaa61c5e258c" -- TESTNET
nftName = fromString "ADAO"
nft = AssetClass (nftSymbol, nftName)

identityMakerSymbol = fromString "947c2a7b45ffdcbc897031daebe5c443805d55ea9a8b4b9711e93a85" -- REAL
-- identityMakerSymbol = fromString "2ab7e0898d0059a7859ed219e7d0e0bfe15e148cd07efaa61c5e258c" -- TESTNET
identityMakerName = fromString "IdentityMaker"
identityMakerClass = AssetClass (identityMakerSymbol, identityMakerName)

propName = "Proposal"
ownName = "Owned"
govName = "GovToken"

myPubKeyHash = fromString "0253cc2bc1ed8176c675f454dd730fae5bfaa147b73924bde70d786a"

treasury = treasuryValidatorHash nft -- ValidatorHash
govSymbol = curSymbol nft
identitySymbol = curSymbol identityMakerClass

govClass = AssetClass (govSymbol, govName)
propClass = AssetClass (identitySymbol, propName)
voteClass = AssetClass (identitySymbol, ownName)

tokenTrace = TokenTrace {tMinted = 1000, treasuryValue = 0}
myOwnership = Ownership {owner = myPubKeyHash, nftSlot = 0, lastTransfer = 16379531770}

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  if nargs /= 0 then
    do
      putStrLn $ "Usage:"
      putStrLn $ "script-dump"
  else 
    do 
      let
        scriptnum = 42
        treasuryname = "treasury.plutus"
        validatorname = "validator.plutus"
        mintGov = "mintGov.plutus"
        mintIdentity = "mintIdentity.plutus"
        appliedValidatorScript = votingPassValidatorScript treasury govClass nft identityMakerClass propClass voteClass
        appliedTreasuryScript = treasuryValidatorScript nft
        appliedMintGov = policy nft
        appliedMintIdentity = policy identityMakerClass

        validatorAsCbor = serialise appliedValidatorScript
        validatorShortBs = SBS.toShort . LB.toStrict $ validatorAsCbor
        validatorScript = PlutusScriptSerialised validatorShortBs

        treasuryAsCbor = serialise appliedTreasuryScript
        treasuryShortBs = SBS.toShort . LB.toStrict $ treasuryAsCbor
        treasuryScript = PlutusScriptSerialised treasuryShortBs

        -- mintingAsValidator = Plutus.Validator $ Plutus.unMintingPolicyScript appliedMintingScript
        mintGovAsValidator = Plutus.Validator $ Plutus.unMintingPolicyScript appliedMintGov
        mintIdAsValidator = Plutus.Validator $ Plutus.unMintingPolicyScript appliedMintIdentity

        mintGovAsCbor = serialise mintGovAsValidator
        mintIdAsCbor = serialise mintIdAsValidator
        -- mintingAsCbor = serialise mintingAsValidator
        mintGovShortBs = SBS.toShort . LB.toStrict $ mintGovAsCbor
        mintIdShortBs = SBS.toShort . LB.toStrict $ mintIdAsCbor
        -- mintingScriptShortBs = SBS.toShort . LB.toStrict $ mintingAsCbor
        mintGovScript = PlutusScriptSerialised mintGovShortBs
        mintIdScript = PlutusScriptSerialised mintIdShortBs
        -- mintingScript = PlutusScriptSerialised mintingScriptShortBs

        aHash = datumHash $ Datum $ toBuiltinData identityMakerClass
        dHash = datumHash $ Datum $ toBuiltinData tokenTrace
        oHash = datumHash $ Datum $ toBuiltinData myOwnership
        datumToEncode = Plutus.builtinDataToData $ toBuiltinData tokenTrace
        ownerToEncode = Plutus.builtinDataToData $ toBuiltinData myOwnership
        encoded = Data.Aeson.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData datumToEncode) 
        ownerEncoded = Data.Aeson.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData ownerToEncode)

      putStrLn $ "Writing output to: " ++ validatorname
      writePlutusScript tokenTrace validatorname validatorScript validatorShortBs
      -- writePlutusScript' scriptnum validatorname validatorScript validatorShortBs

      putStrLn $ "Writing output to: " ++ treasuryname
      writePlutusScript' scriptnum treasuryname treasuryScript treasuryShortBs

      putStrLn $ "Writing output to: " ++ mintGov
      writePlutusScript' scriptnum mintGov mintGovScript mintGovShortBs

      putStrLn $ "Writing output to: " ++ mintIdentity
      writePlutusScript' scriptnum mintIdentity mintIdScript mintIdShortBs

      {-- putStrLn $ "Writing output to: " ++ validVoteName
      writePlutusScript scriptnum validVoteName mintingScript mintingScriptShortBs

      putStrLn $ "Writing output to: " ++ validProposalName
      writePlutusScript scriptnum validProposalName mintingScript mintingScriptShortBs --}

      writeFile "govSymbol.txt" (show $ curSymbol nft)

      writeFile "idSymbol.txt" (show $ curSymbol identityMakerClass)

      writeFile "validator-hash.txt" (show $ votingPassValidatorHash treasury govClass nft identityMakerClass propClass voteClass)

      LB.writeFile "datum.json" encoded
      LB.writeFile "owner.json" ownerEncoded
      writeFile "datum-hash.txt" $ show dHash
      writeFile "asset-hash.txt" $ show aHash
      writeFile "owner-hash.txt" $ show oHash

      writeFile "treasury-hash.txt" $ show treasury

writePlutusScript :: TokenTrace -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript tokenTrace filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataMap [(ScriptDataBytes (fromString "tMinted"), ScriptDataNumber (tMinted tokenTrace)),
                                                               (ScriptDataBytes (fromString "treasuryValue"), ScriptDataNumber (treasuryValue tokenTrace))])
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()

writePlutusScript' :: Integer -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript' scriptnum filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
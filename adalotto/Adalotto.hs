{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}





module Adalotto where

import           Data.Maybe                (fromJust, Maybe (Nothing))
import           Plutus.V1.Ledger.Interval as V1APIL(overlaps)
import           Plutus.V2.Ledger.Api      as V2Api (BuiltinData, PubKeyHash (PubKeyHash),
                                            ScriptContext (scriptContextTxInfo),
                                            TxInfo (txInfoValidRange),
                                            Validator, mkValidatorScript, toBuiltin, POSIXTime, to)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           PlutusTx                  (compile, unstableMakeIsData)
import           PlutusTx.Prelude          ( Bool (False, True), traceIfFalse, ($), (&&), (||) )
import           Prelude                   ( IO,String,Either(Left),Either(Right),error,show, (.), Show, Int )
import           Utilities                 (posixTimeFromIso8601,
                                            printDataToJSON,
                                            wrapValidator, writeValidatorToFile, writeDataToFile)

import           Blockfrost.Client         (runBlockfrost
                                           , getLatestBlock
                                           , Block
                                           , BlockfrostError
                                           , Project (Project, projectEnv, projectId)
                                           , Env (Preview), getTxMetadataByLabelJSON, TxMetaJSON (TxMetaJSON), TxHash, TransactionMetaJSON, getTxMetadataJSON, getTxUtxos, TransactionUtxos)
--import           Blockfrost.API            ()

import qualified Data.ByteString        as BS 
import qualified Data.ByteString.Lazy as BSL (ByteString)
import qualified Plutus.V1.Ledger.Bytes as P
import qualified Blockfrost.API         as BFA (Project)
import qualified Data.Aeson             as AESO (Value)
import Data.Aeson.Types                 ( ToJSON(toJSON), FromJSON ) 
import Data.Text (Text)
import GHC.Generics ( Generic )

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data AdaLottoDatum = AdaLottoDatum
    { signer   :: PubKeyHash
    , tresory  :: PubKeyHash
    , limitOfContract  :: V2Api.POSIXTime
    }  deriving Show

unstableMakeIsData ''AdaLottoDatum

{-# INLINABLE mkLottoValidator #-}
mkLottoValidator :: AdaLottoDatum -> () -> ScriptContext -> Bool
mkLottoValidator dat () ctx =  (traceIfFalse "not the winner signature " signedByWinner &&
                                traceIfFalse "claiming deadline passed" claimDeadLine ) ||
                               (traceIfFalse "deadline not passed" claimBytresory &&
                                traceIfFalse "Not signed by tresory" signedByTresory)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    signedByWinner :: Bool
    signedByWinner = txSignedBy info $ signer dat
    signedByTresory :: Bool
    signedByTresory = txSignedBy info $ tresory dat
    claimDeadLine :: Bool
    claimDeadLine = V1APIL.overlaps (V2Api.to $ limitOfContract dat) $ txInfoValidRange info
    claimBytresory :: Bool
    claimBytresory = if claimDeadLine then False else True

{-# INLINABLE  mkWrappedAdaLottoValidator #-}
mkWrappedAdaLottoValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedAdaLottoValidator = wrapValidator mkLottoValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedAdaLottoValidator ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: IO ()
saveVal = writeValidatorToFile "./contract/AdaLotto.plutus" validator

printAdaLottoDatumJSON :: PubKeyHash -> PubKeyHash -> String  -> IO ()
printAdaLottoDatumJSON tpkh wpkh time  = printDataToJSON $ AdaLottoDatum 
    { signer   = wpkh
    , tresory  = tpkh
    , limitOfContract = fromJust $ posixTimeFromIso8601 time
    }

writeAdaLottoDatumToFile ::PubKeyHash -> PubKeyHash -> String  -> IO ()
writeAdaLottoDatumToFile tpkh wpkh time  = writeDataToFile "./datum/adaLottoDatum.json" $ AdaLottoDatum 
    { signer   = wpkh
    , tresory  = tpkh
    , limitOfContract = fromJust $ posixTimeFromIso8601 time
    }

makeFakeWinnerDatum :: IO()
makeFakeWinnerDatum = writeAdaLottoDatumToFile tresoryPubKeyHash' myPubKeyHash' "2023-05-19T01:00:00.0Z" 
makeFakeWinnerDatum' :: IO()
makeFakeWinnerDatum' = printAdaLottoDatumJSON tresoryPubKeyHash' myPubKeyHash' "2023-05-19T01:00:00.0Z"

 --This function assemble data fund in metadata with blockfrost API
--makeWinnerDatum :: IO()
--makeWinnerDatum = printAdaLottoDatumJSON tresoryPubKeyHash' winnerPubKeyHash deadlineTime


--------------------------------------------------------------------------------------------------
------------------------------------- TRESORY FUNCTION  --------------------------------------------
tresoryPubKeyHash ::BS.ByteString ->  PubKeyHash
tresoryPubKeyHash bs = PubKeyHash $ toBuiltin $ bytesFromHex bs

tresoryPubKeyHash' ::  PubKeyHash
tresoryPubKeyHash' = PubKeyHash $ toBuiltin $ bytesFromHex tresoryHash

tresoryHash :: BS.ByteString
tresoryHash = "d4bcd08ddc025700f9b04a67202d65b87161c4951bfde224e915389b"
--------------------------------------------------------------------------------------------------
------------------------------------- SIGNER FUNCTION --------------------------------------------
winnerPubKeyHash :: BS.ByteString -> PubKeyHash
winnerPubKeyHash bs = PubKeyHash $ toBuiltin $ bytesFromHex bs

myPubKeyHash' ::  PubKeyHash
myPubKeyHash' = PubKeyHash $ toBuiltin $ bytesFromHex myHash

myHash :: BS.ByteString
myHash = "867730ec6fcbcb81cee7b424400c54c1eccff03afd36bf59a691fba8"

bytesFromHex :: BS.ByteString -> BS.ByteString
bytesFromHex = P.bytes . fromEither . P.fromHex
    where
        fromEither (Left e)  = error $ show e
        fromEither (Right b) = b


---------------------------------------------------------------------------------------------------
------------------------------------- BLOCKFROST FUNCTIONS --------------------------------------------

-- initialised a project with api key
bfNetwork :: BFA.Project
bfNetwork =  Project {projectEnv = Preview, projectId = "mFYseTPMINX5aNbZoCEgVwLuXJhLv6fq"}

-- test the connection
bfTest :: BFA.Project -> IO (Either BlockfrostError Block)
bfTest preview = runBlockfrost preview getLatestBlock

-- get list of label you can define
myBfLabels :: Project -> Text -> IO (Either BlockfrostError [TxMetaJSON])
myBfLabels network mylab = runBlockfrost network  $ getTxMetadataByLabelJSON mylab

-- get list of label i choosen
myBfInitTable :: IO (Either BlockfrostError [TxMetaJSON]) --un
myBfInitTable = myBfLabels bfNetwork "713705"

-- get metadataJSON by txhash
myTxMetaJson::Project -> TxHash -> IO (Either BlockfrostError [TransactionMetaJSON]) --search a winner
myTxMetaJson network txhash = runBlockfrost network  $ getTxMetadataJSON txhash

-- extract data from txHash to check with other function if this metadata is sent to the right address "tresory"
myTx::Project -> TxHash -> IO (Either BlockfrostError TransactionUtxos)  --check the receiver address
myTx network txhash = runBlockfrost network  $ getTxUtxos txhash


-- unwrap either to take list of TxMetaJSON
giveMeTxMetaData :: Either BlockfrostError [TxMetaJSON] ->  [TxMetaJSON] -- deux
giveMeTxMetaData (Right txmetalist) = txmetalist
giveMeTxMetaData (Left _ )          = []

takefirst :: [TxMetaJSON] -> TxMetaJSON --trois
takefirst (x:_)  = x
takefirst []      = TxMetaJSON "rien" Nothing

takelast :: [TxMetaJSON] -> TxMetaJSON --trois
takelast [x] = x
takelast (_:xs)  = takelast xs
takelast []  = TxMetaJSON "rien" Nothing

myValue :: TxMetaJSON ->  AESO.Value  --sample
myValue = toJSON

--------------------------------------------------------------------------------------------------
------------------------------------- JSON FUNCTIONS --------------------------------------------

newtype Ticket  = Ticket 
    {ticket :: [Int] } deriving (Show, Generic, ToJSON, FromJSON )

--------------------------------------------------------------------------------------------------
------------------------------------- TEST VALUE --------------------------------------------

myTime1 :: String
myTime1 = "2023-05-12T16:00:00.0Z"
myTime2 :: String
myTime2 = "2023-07-14T16:00:00.0Z"
myTime3 :: String
myTime3 = "2023-06-18T16:00:00.0Z"
myEndOfContract1 :: String
myEndOfContract1 = "2023-05-24T23:00:00.0Z"
myEndOfContract2 :: String
myEndOfContract2 = "2023-05-29T20:00:00.0Z"

myTx1 :: V2Api.POSIXTime
myTx1 = fromJust $ posixTimeFromIso8601 myTime1 --12 mai
myTx2 :: V2Api.POSIXTime
myTx2 = fromJust $ posixTimeFromIso8601 myTime2 -- 14 Juillet
myTx3 :: V2Api.POSIXTime
myTx3 = fromJust $ posixTimeFromIso8601 myTime3 -- 18 juin
myEnd1 :: V2Api.POSIXTime
myEnd1 = fromJust $ posixTimeFromIso8601 myEndOfContract1 --24 mai
myEnd2 :: V2Api.POSIXTime
myEnd2 = fromJust $ posixTimeFromIso8601 myEndOfContract2 

myPosixTime :: V2Api.POSIXTime
myPosixTime = 1683907200000

myTxHash :: TxHash
myTxHash = "00a0ee3a5436609aabeb4847a1cfbd9d061ac57bcf36b91917a9fdffe3c25992"

myAdaLotto :: AdaLottoDatum
myAdaLotto =AdaLottoDatum tresoryPubKeyHash' myPubKeyHash' myTx1 

jsonSample :: BSL.ByteString
jsonSample = "{\"ticket\": [1,8,11,27,34,42]}"

ticketSample :: Ticket 
ticketSample = Ticket [1,8,11,27,34,42]
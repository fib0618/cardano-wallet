{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- This module provides the main wallet server function for the
-- Jörmungandr backend.
--
-- The "Cardano.Wallet.Jormungandr.Network" module handles synchronization with
-- the @jormungandr@ process.
--
-- The block format decoders are in "Cardano.Wallet.Jormungandr.Network".
--
-- Functionality specific to this backend for creating transactions is in
-- "Cardano.Wallet.Jormungandr.Transaction".

module Cardano.Wallet.Shelley
    ( serveWallet

      -- * Tracing
    , ServerLog (..)
    ) where

import Prelude


import Cardano.BM.Trace
    ( Trace, appendName, logInfo )
import Cardano.CLI
    ( Port (..) )
import Cardano.Launcher
    ( installSignalHandlers )
import Cardano.Pool.Metrics
    ( StakePoolLayer (..) )
import Cardano.Wallet.Api
    ( ApiLayer )
import Cardano.Wallet.Api.Server
    ( HostPreference, Listen (..) )
import Cardano.Wallet.Api.Types
    ( DecodeAddress, EncodeAddress )
import Cardano.Wallet.DaedalusIPC
    ( daedalusIPC )
import Cardano.Wallet.DB.Sqlite
    ( PersistState )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress
    , Depth (..)
    , NetworkDiscriminant (..)
    , NetworkDiscriminantVal
    , PaymentAddress
    , PersistPrivateKey
    , WalletKey
    , networkDiscriminantVal
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState )
import Cardano.Wallet.Primitive.Types
    ( Address
    , Block
    , BlockHeader (..)
    , BlockchainParameters (..)
    , ChimericAccount
    , SyncTolerance
    )
import Cardano.Wallet.Shelley.Network
    ( UseRunningOrLaunch, withNetworkLayer )
import Cardano.Wallet.Shelley.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Transaction
    ( ErrValidateSelection, TransactionLayer )
import Control.Concurrent.Async
    ( race )
import Control.DeepSeq
    ( NFData )
import Control.Tracer
    ( contramap )
import Data.Functor
    ( ($>) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Void
    ( Void )
import Fmt
    ( Buildable (..) )
import Network.Socket
    ( SockAddr, Socket )
import Network.Wai.Middleware.Logging
    ( ApiLog )
import System.Exit
    ( ExitCode (..) )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.UTxO as CC
import qualified Cardano.Crypto as CC
import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.ByteArray as BA
import qualified Data.List.NonEmpty as NE
import qualified Network.Wai.Handler.Warp as Warp
import qualified Ouroboros.Consensus.Ledger.Byron as O
import qualified Ouroboros.Network.Block as O

data Shelley

-- | The @cardano-wallet-jormungandr@ main function. It takes the configuration
-- which was passed from the CLI and environment and starts all components of
-- the wallet.
serveWallet
    :: forall (n :: NetworkDiscriminant) t.
        ( NetworkDiscriminantVal n
        , DecodeAddress n
        , EncodeAddress n
        , DelegationAddress n ShelleyKey
        , PaymentAddress n ByronKey
        , t ~ Shelley
        )
    => (CM.Configuration, Trace IO ServerLog)
    -- ^ Logging config.
    -> SyncTolerance
    -- ^ A time tolerance within we consider being synced
    -> Maybe FilePath
    -- ^ Database folder filepath
    -> HostPreference
    -- ^ Which host to bind.
    -> Listen
    -- ^ HTTP API Server port.
    -> UseRunningOrLaunch
    -- ^ Whether and how to launch or use the node backend.
    -> (SockAddr -> Port "node" -> BlockchainParameters -> IO ())
    -- ^ Callback to run before the main loop
    -> IO ExitCode
serveWallet
        (cfg, tr) sTolerance databaseDir hostPref listen lj _beforeMainLoop = do
    -- TODO: We are not calling beforeMainLoop. We don't have a port to pass in.
    installSignalHandlers trText
    logInfo trText "Wallet backend server starting..."
    logInfo trText $ "Haskell node on " <> toText (networkDiscriminantVal @n)
    Server.withListeningSocket hostPref listen $ \case
        Left e -> print e $> ExitFailure 1 -- TODO
        Right (wPort, socket) -> do
            let tracerIPC = appendName "daedalus-ipc" trText
            either id id <$> race
                (daedalusIPC tracerIPC wPort $> ExitSuccess)
                (serveApp socket)

  where
    trText = contramap (fmap LogText) tr
    serveApp socket = withNetworkLayer trText lj $ \case
        Left _e -> error "handleNetworkStartupError e"
        Right (_cp, nl) -> do
            let (_, bp) = staticBlockchainParameters nl
            let rndTl = newTransactionLayer (getGenesisBlockHash bp)
            let seqTl = newTransactionLayer (getGenesisBlockHash bp)
            let spl = dummyStakePoolLayer
            rndApi  <- apiLayer trText rndTl (toWalletBlock <$> nl)
            seqApi  <- apiLayer trText seqTl (toWalletBlock <$> nl)
            let tr' = contramap (fmap LogApiServerMsg) tr
            print ("hi serveApp"::String)
            startServer tr' socket bp rndApi seqApi spl
            pure ExitSuccess

    startServer
        :: Trace IO ApiLog
        -> Socket
        -> BlockchainParameters
        -> ApiLayer (RndState 'Mainnet) t ByronKey
        -> ApiLayer (SeqState n ShelleyKey) t ShelleyKey
        -> StakePoolLayer IO
        -> IO ()
    startServer tracer socket _bp rndApi seqApi poolApi = do
        let tracerApi = appendName "api" tracer
        let settings = Warp.defaultSettings
        Server.start settings tracerApi socket rndApi seqApi poolApi

    apiLayer
        :: forall s k.
            ( IsOurs s Address
            , IsOurs s ChimericAccount
            , NFData s
            , Show s
            , PersistState s
            , PersistPrivateKey (k 'RootK)
            , WalletKey k
            )
        => Trace IO Text
        -> TransactionLayer t k
        -> NetworkLayer IO Block
        -> IO (ApiLayer s t k)
    apiLayer tracer tl nl = do
        let (block0, bp) = staticBlockchainParameters nl
        wallets <- maybe (pure []) (Sqlite.findDatabases @k trText) databaseDir
        db <- Sqlite.newDBFactory cfg trText databaseDir
        Server.newApiLayer
            tracer (block0, bp, sTolerance) nl tl db wallets

    dummyStakePoolLayer :: StakePoolLayer IO
    dummyStakePoolLayer = StakePoolLayer
        { listStakePools = return []
        , knownStakePools = return []
        }



toWalletBlock :: O.ByronBlock -> W.Block
toWalletBlock b = W.Block
    { header = convertBlockHeader b
    , transactions =
        case O.byronBlockRaw b  of
            CC.ABOBBlock cb ->
                let
                    body = CC.blockBody cb
                    txPay = CC.bodyTxPayload body
                    txAuxs = CC.aUnTxPayload txPay
                    txs = map CC.taTx txAuxs
                in
                    map convertTx txs
            CC.ABOBBoundary _ ->
                error "Our friend the Epoch Boundary Block!"
    , delegations = []
    }

convertTx :: CC.Tx -> W.Tx
convertTx tx = W.Tx
    { W.txId = convertHash $ CC.hash tx
    , W.resolvedInputs = map convertTxIn $ NE.toList $ CC.txInputs tx
    , W.outputs = map convertTxOut $ NE.toList $ CC.txOutputs tx
    }
  where
    convertHash = W.Hash . BA.convert

    convertTxIn (CC.TxInUtxo txH idx) = (W.TxIn (convertHash txH) idx, coin)
       where
          coin = W.Coin 0
          -- TODO: error "1. We can't get the coin of Byron inputs!"

    convertTxOut o = W.TxOut
        (convertAddr $ CC.txOutAddress o)
        (convertCoin $ CC.txOutValue o)
      where
        convertAddr = error "todo use toBinary"
        -- Byron addresses should be no problem. I'm not sure how Shelley
        -- addresses will look.
        convertCoin = W.Coin . CC.unsafeGetLovelace

convertBlockHeader :: O.ByronBlock -> W.BlockHeader
convertBlockHeader b = W.BlockHeader
    { W.slotId = convertSlot (O.blockSlot b)

    , W.blockHeight = convertBlockNo $ O.blockNo b
    , W.headerHash = convertHash $ O.blockHash b
    , W.parentHeaderHash = convertChainHash $ O.blockPrevHash b
    }
  where
    convertHash = W.Hash . BA.convert . O.unByronHash

    -- The cardano-ledger equivalent is EpochAndSlotCount, i.e epoch
    -- number and the slot counted from the start of the epoch.
    --
    -- But block headers only contain the slot number (SlotNo), counted
    -- from genesis.
    --
    -- Assuming the epoch-length doesn't change, we can easily do the
    -- conversion. The SlotId isn't interesting to the wallet, so long term, we
    -- should concider using SlotNo instead.
    convertSlot = W.fromFlatSlot (W.EpochLength 21600) . O.unSlotNo

    convertChainHash x = case x of
        O.BlockHash h ->
            convertHash h
        O.GenesisHash ->
            error "how do we represent the genesis hash?"
            -- Seems like a minor problem.
    convertBlockNo = Quantity . fromIntegral . O.unBlockNo

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

-- | The type of trace events produced by the Jörmungandr API server.
data ServerLog
    = LogApiServerMsg ApiLog
    | LogText Text
    deriving (Show)

instance ToText ServerLog where
    toText msg = case msg of
        LogApiServerMsg load -> toText load
        LogText txt -> txt


-- TODO: Give this a proper name and structure
-- TODO: Move to Transaction module
data ErrShelleyValidateSelection = ErrShelleyValidateSelection
    deriving (Eq, Show)

instance Buildable ErrShelleyValidateSelection where
    build _ = build ("<ErrShelleyValidateSelection (to be implemented)>" :: String)

type instance ErrValidateSelection Shelley = Void -- ?

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2020 IOHK
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

module Cardano.Wallet.Jormungandr
    ( serveWallet

      -- * Utilities
    , toSPBlock

      -- * Tracing
    , Tracers' (..)
    , Tracers
    , TracerSeverities
    , tracerLabels
    , tracerDescriptions
    , nullTracers
    , setupTracers
    , tracerSeverities
    ) where

import Prelude

import Cardano.BM.Data.LogItem
    ( LoggerName )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( DefinePrivacyAnnotation (..), DefineSeverity (..) )
import Cardano.BM.Trace
    ( Trace, appendName )
import Cardano.CLI
    ( Port (..) )
import Cardano.DB.Sqlite
    ( DBLog )
import Cardano.Launcher
    ( ProcessHasExited (..), installSignalHandlers )
import Cardano.Pool.Metrics
    ( StakePoolLayer
    , StakePoolLayerLog (MsgStakePoolWorker)
    , StakePoolMonitorLog
    , monitorStakePools
    , newStakePoolLayer
    )
import Cardano.Wallet.Api
    ( ApiLayer )
import Cardano.Wallet.Api.Server
    ( HostPreference, Listen (..), ListenError (..) )
import Cardano.Wallet.Api.Types
    ( DecodeAddress, EncodeAddress )
import Cardano.Wallet.DaedalusIPC
    ( DaedalusIPCLog, daedalusIPC )
import Cardano.Wallet.DB.Sqlite
    ( DatabasesStartupLog, PersistState )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Network
    ( BaseUrl (..)
    , ErrGetBlockchainParams (..)
    , ErrStartup (..)
    , JormungandrBackend (..)
    , JormungandrConnParams (..)
    , NetworkLayerLog
    , withNetworkLayer
    )
import Cardano.Wallet.Jormungandr.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Logging
    ( filterTraceSeverity, fromLogObject, trMessageText )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress
    , Depth (..)
    , NetworkDiscriminant (..)
    , NetworkDiscriminantVal
    , PersistPrivateKey
    , WalletKey
    , networkDiscriminantVal
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
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
import Cardano.Wallet.Registry
    ( WorkerRegistryLog, defaultWorkerAfter )
import Cardano.Wallet.Transaction
    ( TransactionLayer )
import Control.Applicative
    ( Const (..) )
import Control.Concurrent
    ( forkFinally )
import Control.Concurrent.Async
    ( race )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( void )
import Control.Tracer
    ( Tracer (..), nullTracer, traceWith )
import Data.Function
    ( (&) )
import Data.Functor
    ( ($>) )
import Data.Functor.Contravariant
    ( contramap )
import Data.Maybe
    ( fromMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..), showT )
import GHC.Generics
    ( Generic )
import Network.Socket
    ( SockAddr, Socket, getSocketName )
import Network.Wai.Handler.Warp
    ( setBeforeMainLoop )
import Network.Wai.Middleware.Logging
    ( ApiLog )
import System.Exit
    ( ExitCode (..) )
import System.IO.Temp
    ( withSystemTempDirectory )

import qualified Cardano.Pool.DB as Pool
import qualified Cardano.Pool.DB.Sqlite as Pool
import qualified Cardano.Pool.Metrics as Pool
import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.Jormungandr.Binary as J
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp

-- | The @cardano-wallet-jormungandr@ main function. It takes the configuration
-- which was passed from the CLI and environment and starts all components of
-- the wallet.
serveWallet
    :: forall (n :: NetworkDiscriminant) t.
        ( t ~ Jormungandr
        , NetworkDiscriminantVal n
        , DecodeAddress n
        , EncodeAddress n
        , DelegationAddress n ShelleyKey
        )
    => Tracers IO
    -- ^ Logging config.
    -> SyncTolerance
    -- ^ A time tolerance within we consider being synced
    -> Maybe FilePath
    -- ^ Database folder filepath
    -> HostPreference
    -- ^ Which host to bind.
    -> Listen
    -- ^ HTTP API Server port.
    -> JormungandrBackend
    -- ^ Whether and how to launch or use the node backend.
    -> (SockAddr -> Port "node" -> BlockchainParameters -> IO ())
    -- ^ Callback to run before the main loop
    -> IO ExitCode
serveWallet
    Tracers{..} sTolerance databaseDir hostPref listen lj beforeMainLoop = do
    installSignalHandlers (traceWith applicationTracer MsgSigTerm)
    traceWith applicationTracer MsgStarting
    traceWith applicationTracer $ MsgNetworkName $ networkDiscriminantVal @n
    Server.withListeningSocket hostPref listen $ \case
        Left e -> handleApiServerStartupError e
        Right (wPort, socket) -> do
            either id id <$> race
                (daedalusIPC daedalusIPCTracer wPort $> ExitSuccess)
                (serveApp socket)

  where
    serveApp socket = withNetworkLayer networkTracer lj $ \case
        Left e -> handleNetworkStartupError e
        Right (cp, nl) -> do
            let nPort = Port $ baseUrlPort $ _restApi cp
            let (_, bp) = staticBlockchainParameters nl
            let byronTl = newTransactionLayer (getGenesisBlockHash bp)
            let icarusTl = newTransactionLayer (getGenesisBlockHash bp)
            let shelleyTl = newTransactionLayer (getGenesisBlockHash bp)
            let poolDBPath = Pool.defaultFilePath <$> databaseDir
            Pool.withDBLayer stakePoolDBTracer poolDBPath $ \db ->
                withSystemTempDirectory "stake-pool-metadata" $ \md -> do
                    poolApi <- stakePoolLayer nl db md
                    byronApi <- apiLayer "byron" byronTl nl
                    icarusApi <- apiLayer "icarus" icarusTl nl
                    shelleyApi <- apiLayer "shelley" shelleyTl nl
                    startServer socket nPort bp
                        byronApi
                        icarusApi
                        shelleyApi
                        poolApi
                    pure ExitSuccess

    startServer
        :: Socket
        -> Port "node"
        -> BlockchainParameters
        -> ApiLayer (RndState 'Mainnet) t ByronKey
        -> ApiLayer (SeqState 'Mainnet IcarusKey) t IcarusKey
        -> ApiLayer (SeqState n ShelleyKey) t ShelleyKey
        -> StakePoolLayer IO
        -> IO ()
    startServer socket nPort bp byron icarus shelley pools = do
        sockAddr <- getSocketName socket
        let settings = Warp.defaultSettings
                & setBeforeMainLoop (beforeMainLoop sockAddr nPort bp)
        Server.start settings apiServerTracer socket byron icarus shelley pools

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
        => LoggerName
        -> TransactionLayer t k
        -> NetworkLayer IO t J.Block
        -> IO (ApiLayer s t k)
    apiLayer _loggerName tl nl = do
        let (block0, bp) = staticBlockchainParameters nl
        let tracer = contramap MsgDatabaseStartup applicationTracer
        wallets <- maybe (pure []) (Sqlite.findDatabases @k tracer) databaseDir
        db <- Sqlite.newDBFactory walletDbTracer databaseDir
        Server.newApiLayer
            walletEngine (toWLBlock block0, bp, sTolerance) nl' tl db wallets
      where
        nl' = toWLBlock <$> nl

    stakePoolLayer
        :: NetworkLayer IO t J.Block
        -> Pool.DBLayer IO
        -> FilePath
        -> IO (StakePoolLayer IO)
    stakePoolLayer nl db metadataDir = do
        void $ forkFinally (monitorStakePools stakePoolMonitorTracer nl' db) onExit
        pure $ newStakePoolLayer stakePoolLayerTracer db nl' metadataDir
      where
        nl' = toSPBlock <$> nl
        onExit = defaultWorkerAfter
            (fromLogObject $ contramap MsgStakePoolWorker stakePoolLayerTracer)

    handleNetworkStartupError :: ErrStartup -> IO ExitCode
    handleNetworkStartupError err = do
        traceWith applicationTracer $ MsgWalletStartupError err
        pure $ ExitFailure $ exitCodeNetwork err

    handleApiServerStartupError :: ListenError -> IO ExitCode
    handleApiServerStartupError err = do
        traceWith applicationTracer $ MsgServerStartupError err
        pure $ ExitFailure $ exitCodeApiServer err

--------------------------------------------------------------------------------
-- Exported Utilities
--------------------------------------------------------------------------------

-- | Covert a raw block to one that the "Cardano.Pool.Metrics" module accepts.
toSPBlock :: J.Block -> Pool.Block
toSPBlock b = Pool.Block
     (convertHeader header)
     -- FIXME
     -- Allow defining different types for block vs genesis block in the network
     -- layer so that staticBlockchainParameter isn't partial.
     (fromMaybe (error "block has no producer") $ J.producedBy header)
     (J.poolRegistrationsFromBlock b)
   where
     header = J.header b
     convertHeader :: J.BlockHeader -> BlockHeader
     convertHeader h = BlockHeader
         (J.slot h)
         (Quantity $ fromIntegral $ J.chainLength h)
         (J.headerHash h)
         (J.parentHeaderHash h)

toWLBlock :: J.Block -> Block
toWLBlock = J.convertBlock

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

-- | Log messages related to application startup and shutdown.
data ApplicationLog
    = MsgStarting
    | MsgNetworkName NetworkDiscriminant
    | MsgSigTerm
    | MsgWalletStartupError ErrStartup
    | MsgServerStartupError ListenError
    | MsgDatabaseStartup DatabasesStartupLog
    deriving (Generic, Show, Eq)

instance ToText ApplicationLog where
    toText msg = case msg of
        MsgStarting -> "Wallet backend server starting..."
        MsgNetworkName n -> "Node is Jörmungandr on " <> toText n
        MsgSigTerm -> "Terminated by signal."
        MsgDatabaseStartup dbMsg -> toText dbMsg
        MsgWalletStartupError startupErr -> case startupErr of
            ErrStartupGetBlockchainParameters e -> case e of
                ErrGetBlockchainParamsNetworkUnreachable _ ->
                    "It looks like Jörmungandr is down? Hint: double-check\
                    \  Jörmungandr server's port."
                ErrGetBlockchainParamsGenesisNotFound block0H ->
                    T.pack $ mconcat
                    [ "Failed to retrieve the genesis block. The block doesn't"
                    , " exist! Hint: double-check the genesis hash you've just "
                    , " gave me via '--genesis-block-hash'"
                    , "(i.e. " <> showT block0H <> ")."
                    ]
                ErrGetBlockchainParamsIncompleteParams _ -> mempty
                    <> "I successfully retrieved the genesis block from "
                    <> "Jörmungandr, but there's no initial fee policy defined?"
            ErrStartupInvalidGenesisBlock file -> mempty
                    <> "As far as I can tell, this isn't a valid block file: "
                    <> T.pack file
            ErrStartupInvalidGenesisHash h -> mempty
                    <> "As far as I can tell, this isn't a valid block hash: "
                    <> T.pack h
            ErrStartupCommandExited pe -> case pe of
                ProcessDidNotStart _cmd exc ->
                    "Could not start the node backend. " <> T.pack (show exc)
                ProcessHasExited _cmd st ->
                    "The node exited with status " <> T.pack (show st)
            ErrStartupNodeNotListening -> mempty
                    <> "Waited too long for Jörmungandr to become available. "
                    <> "Giving up!"
        MsgServerStartupError startupErr -> case startupErr of
            ListenErrorHostDoesNotExist host -> mempty
                    <> "Can't listen on "
                    <> T.pack (show host)
                    <> ". It does not exist."
            ListenErrorInvalidAddress host -> mempty
                    <> "Can't listen on "
                    <> T.pack (show host)
                    <> ". Invalid address."
            ListenErrorAddressAlreadyInUse mPort -> mempty
                    <> "The API server listen port "
                    <> maybe "(unknown)" (T.pack . show) mPort
                    <> " is already in use."
            ListenErrorOperationNotPermitted -> mempty
                    <> "Cannot listen on the given port. "
                    <> "The operation is not permitted."

instance DefinePrivacyAnnotation ApplicationLog
instance DefineSeverity ApplicationLog where
    defineSeverity ev = case ev of
        MsgStarting -> Info
        MsgSigTerm -> Notice
        MsgNetworkName _ -> Info
        MsgDatabaseStartup dbEv -> defineSeverity dbEv
        MsgWalletStartupError _ -> Alert
        MsgServerStartupError _ -> Alert

-- | Failure status codes for network backend errors.
exitCodeNetwork :: ErrStartup -> Int
exitCodeNetwork = \case
    ErrStartupGetBlockchainParameters e -> case e of
        ErrGetBlockchainParamsNetworkUnreachable _ -> 30
        ErrGetBlockchainParamsGenesisNotFound _ -> 31
        ErrGetBlockchainParamsIncompleteParams _ -> 32
    ErrStartupInvalidGenesisBlock _ -> 33
    ErrStartupInvalidGenesisHash _ -> 34
    ErrStartupCommandExited pe -> case pe of
        ProcessDidNotStart _ _ -> 40
        ProcessHasExited _ _ -> 41
    ErrStartupNodeNotListening -> 42

-- | Failure status codes for HTTP API server errors.
exitCodeApiServer :: ListenError -> Int
exitCodeApiServer = \case
    ListenErrorHostDoesNotExist _ -> 10
    ListenErrorInvalidAddress _ -> 11
    ListenErrorAddressAlreadyInUse _ -> 12
    ListenErrorOperationNotPermitted -> 13

{-------------------------------------------------------------------------------
                                    Tracers
-------------------------------------------------------------------------------}

-- | The types of trace events produced by the Jörmungandr API server.
data Tracers' f = Tracers
    { applicationTracer      :: f ApplicationLog
    , apiServerTracer        :: f ApiLog
    , walletEngine           :: f WorkerRegistryLog
    , walletDbTracer         :: f DBLog
    , networkTracer          :: f NetworkLayerLog
    , stakePoolMonitorTracer :: f StakePoolMonitorLog
    , stakePoolLayerTracer   :: f StakePoolLayerLog
    , stakePoolDBTracer      :: f DBLog
    , daedalusIPCTracer      :: f DaedalusIPCLog
    }

-- | All of the Jörmungandr 'Tracer's.
type Tracers m = Tracers' (Tracer m)

-- | The minimum severities for 'Tracers'. 'Nothing' indicates that tracing is
-- completely disabled.
type TracerSeverities = Tracers' (Const (Maybe Severity))

-- | Construct a 'TracerSeverities' record with all tracers set to the given
-- severity.
tracerSeverities :: Maybe Severity -> TracerSeverities
tracerSeverities sev = Tracers
    { applicationTracer      = Const sev
    , apiServerTracer        = Const sev
    , walletDbTracer         = Const sev
    , walletEngine           = Const sev
    , networkTracer          = Const sev
    , stakePoolMonitorTracer = Const sev
    , stakePoolLayerTracer   = Const sev
    , stakePoolDBTracer      = Const sev
    , daedalusIPCTracer      = Const sev
    }

-- | Set up tracing with textual log messages.
setupTracers :: TracerSeverities -> Trace IO Text -> Tracers IO
setupTracers sev tr = Tracers
    { applicationTracer      = mkTrace applicationTracer      $ onoff applicationTracer      tr
    , apiServerTracer        = mkTrace apiServerTracer        $ onoff apiServerTracer        tr
    , walletEngine           = mkTrace walletEngine           $ onoff walletEngine           tr
    , walletDbTracer         = mkTrace walletDbTracer         $ onoff walletDbTracer         tr
    , networkTracer          = mkTrace networkTracer          $ onoff networkTracer          tr
    , stakePoolMonitorTracer = mkTrace stakePoolMonitorTracer $ onoff stakePoolMonitorTracer tr
    , stakePoolLayerTracer   = mkTrace stakePoolLayerTracer   $ onoff stakePoolLayerTracer   tr
    , stakePoolDBTracer      = mkTrace stakePoolDBTracer      $ onoff stakePoolDBTracer      tr
    , daedalusIPCTracer      = mkTrace daedalusIPCTracer      $ onoff daedalusIPCTracer      tr
    }
  where
    onoff
        :: Monad m
        => (TracerSeverities -> Const (Maybe Severity) a)
        -> Trace m b
        -> Trace m b
    onoff f = case getConst (f sev) of
        Nothing -> const nullTracer
        Just s -> filterTraceSeverity s

    mkTrace
        :: (DefinePrivacyAnnotation a, DefineSeverity a, ToText a)
        => (Tracers' (Const Text) -> Const Text a)
        -> Trace IO Text
        -> Tracer IO a
    mkTrace f = trMessageText . appendName (getConst $ f tracerLabels)

-- | Strings that the user can refer to tracers by.
tracerLabels :: Tracers' (Const Text)
tracerLabels = Tracers
    { applicationTracer      = Const "application"
    , apiServerTracer        = Const "api-server"
    , walletEngine           = Const "wallet-engine"
    , walletDbTracer         = Const "wallet-db"
    , networkTracer          = Const "network"
    , stakePoolMonitorTracer = Const "stake-pool-monitor"
    , stakePoolLayerTracer   = Const "stake-pool-layer"
    , stakePoolDBTracer      = Const "stake-pool-db"
    , daedalusIPCTracer      = Const "daedalus-ipc"
    }

-- | Names and descriptions of the tracers, for user documentation.
tracerDescriptions :: [(String, String)]
tracerDescriptions =
    [ ( lbl applicationTracer
      , "About start-up logic and the server's surroundings."
      )
    , ( lbl apiServerTracer
      , "About the HTTP API requests and responses."
      )
    , ( lbl walletEngine
      , "About background wallet workers events and core wallet engine."
      )
    , ( lbl walletDbTracer
      , "About database operations of each wallet."
      )
    , ( lbl networkTracer
      , "About networking communications with the node."
      )
    , ( lbl stakePoolMonitorTracer
      , "About the background worker monitoring stake pools."
      )
    , ( lbl stakePoolLayerTracer
      , "About operations on stake pools."
      )
    , ( lbl stakePoolDBTracer
      , "About database operations of the stake pools db."
      )
    , ( lbl daedalusIPCTracer
      , "About inter-process communications with Daedalus."
      )
    ]
  where
    lbl f = T.unpack . getConst . f $ tracerLabels

-- | Use a 'nullTracer' for each of the 'Tracer's in 'Tracers'
nullTracers :: Monad m => Tracers m
nullTracers = Tracers
    { applicationTracer      = nullTracer
    , apiServerTracer        = nullTracer
    , walletEngine           = nullTracer
    , walletDbTracer         = nullTracer
    , networkTracer          = nullTracer
    , stakePoolMonitorTracer = nullTracer
    , stakePoolLayerTracer   = nullTracer
    , stakePoolDBTracer      = nullTracer
    , daedalusIPCTracer      = nullTracer
    }

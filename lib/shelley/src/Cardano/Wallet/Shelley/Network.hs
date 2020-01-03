{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
--{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-redundant-constraints
--   -fno-warn-unused-matches -fno-warn-unused-local-binds #-}

{-
Good to read before / additional resources:

- Module's documentation in `ouroboros-network/typed-protocols/src/Network/TypedProtocols.hs`
- Data Diffusion and Peer Networking in Shelley (see: https://raw.githubusercontent.com/wiki/input-output-hk/cardano-wallet/data_diffusion_and_peer_networking_in_shelley.pdf)
    - In particular sections 4.1, 4.2, 4.6 and 4.8


-}

module Cardano.Wallet.Shelley.Network
    ( point1
      -- * Top-Level Interface
    , ChainParameters (..)
    , withNetworkLayer
    , UseRunningOrLaunch (..)
    , LaunchConfig (..)
    , ConnectionParams (..)
    , mainnetConnectionParams
    , Network (..)
    , byronBlockchainParameters

    -- * Re-Export
    , EpochSlots (..)
    , ProtocolMagicId (..)

      -- * Constructors
    , connectClient
    , dummyNodeToClientVersion

      -- * Transport Helpers
    , localSocketAddrInfo
    , localSocketFilePath
    ) where

import Prelude

import Cardano.BM.Trace
    ( Trace, nullTracer )
import Cardano.Chain.Slotting
    ( EpochSlots (..) )
import Cardano.Crypto
    ( ProtocolMagicId (..) )
import Cardano.Launcher
    ( Command (..), StdStream, withBackendProcess )
import Cardano.Wallet.Logging
    ( transformTextTrace )
import Cardano.Wallet.Network
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , BlockHeader (..)
    , BlockchainParameters (..)
    , EpochLength (..)
    , FeePolicy (..)
    , Hash (..)
    , SlotId (..)
    , SlotLength (..)
    , StartTime (..)
    , flatSlot
    , fromFlatSlot
    )
import Codec.SerialiseTerm
    ( CodecCBORTerm )
import Control.Exception
    ( catch, throwIO )
import Control.Monad.Class.MonadST
    ( MonadST )
import Control.Monad.Class.MonadThrow
    ( MonadThrow )
import Control.Monad.Class.MonadTimer
    ( MonadTimer )
import Control.Tracer
    ( Tracer, contramap )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Data.Void
    ( Void )
import Data.Word
    ( Word16 )
import Network.Mux.Interface
    ( AppType (..) )
import Network.Mux.Types
    ( MuxError )
import Network.Socket
    ( AddrInfo (..), Family (..), SockAddr (..), SocketType (..) )
import Network.TypedProtocol.Channel
    ( Channel )
import Network.TypedProtocol.Codec
    ( Codec )
import Network.TypedProtocol.Codec.Cbor
    ( DeserialiseFailure )
import Network.TypedProtocol.Driver
    ( TraceSendRecv, runPeer )
import Ouroboros.Consensus.Ledger.Byron
    ( ByronBlock (..)
    , ByronHash (..)
    , GenTx
    , decodeByronBlock
    , decodeByronGenTx
    , decodeByronHeaderHash
    , encodeByronBlock
    , encodeByronGenTx
    , encodeByronHeaderHash
    )
import Ouroboros.Consensus.NodeId
    ( CoreNodeId (..) )
import Ouroboros.Network.Block
    ( Point, Tip (..) )
import Ouroboros.Network.Magic
    ( NetworkMagic (..) )
import Ouroboros.Network.Mux
    ( OuroborosApplication (..) )
import Ouroboros.Network.NodeToClient
    ( ConnectionId (..)
    , NetworkConnectTracers (..)
    , NodeToClientProtocols (..)
    , NodeToClientVersion (..)
    , NodeToClientVersionData (..)
    , connectTo
    , localTxSubmissionClientNull
    , nodeToClientCodecCBORTerm
    )
import Ouroboros.Network.Protocol.ChainSync.Client
    ( ChainSyncClient (..)
    , ClientStIdle (..)
    , ClientStIntersect (..)
    , ClientStNext (..)
    , chainSyncClientPeer
    )
import Ouroboros.Network.Protocol.ChainSync.Codec
    ( codecChainSync )
import Ouroboros.Network.Protocol.ChainSync.Type
    ( ChainSync )
import Ouroboros.Network.Protocol.Handshake.Version
    ( DictVersion (..), simpleSingletonVersions )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( LocalTxSubmissionClient (..), localTxSubmissionClientPeer )
import Ouroboros.Network.Protocol.LocalTxSubmission.Codec
    ( codecLocalTxSubmission )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( LocalTxSubmission )

import qualified Cardano.Crypto as CC
import qualified Codec.Serialise as CBOR
import qualified Crypto.Hash as Crypto
import qualified Network.Socket as Socket
import qualified Ouroboros.Network.Block as O
import qualified Ouroboros.Network.Point as Point


-- A point where there is high activity on the mainnet
point1 :: Point ByronBlock
point1 =
    let
        Right h = CC.decodeAbstractHash "8a7505ec268a934c9c532fcf21185baf849c0de04a6d12cdf80b0fc45fe6ecb3"
        slot = 21600*19+6540

    in O.Point $ Point.block
        (O.SlotNo slot)
        (ByronHash h)

--
--
--

data Network = Mainnet | Testnet

-- | Whether to start Jormungandr with the given config, or to connect to an
-- already running Jormungandr REST API using the given parameters.
data UseRunningOrLaunch
    = UseRunning ConnectionParams
    | Launch LaunchConfig
    deriving (Show, Eq)

-- | Parameters for connecting to a Jormungandr REST API.
data ConnectionParams = ConnectionParams
    { block0H :: Hash "Genesis" -- NOTE: Do we really need this?
    , chainParams :: ChainParameters
    , socket :: AddrInfo -- ^ Socket for communicating with the node
    }
    deriving (Show, Eq)

-- | A subset of the Jormungandr configuration parameters, used for starting the
-- Jormungandr node backend.
newtype LaunchConfig = LaunchConfig
    { outputStream :: StdStream
    }
    deriving (Show, Eq)

data ErrStartup

-- | Starts the network layer and runs the given action with a
-- 'NetworkLayer'. The caller is responsible for handling errors which may have
-- occurred while starting the Node.
withNetworkLayer
    :: forall a . ()
    => Trace IO Text
    -- ^ Logging
    -> UseRunningOrLaunch
    -- ^ How Jörmungandr is started.
    -> (Either ErrStartup (ConnectionParams, NetworkLayer IO ByronBlock) -> IO a)
    -- ^ The action to run. It will be passed the connection parameters used,
    -- and a network layer if startup was successful.
    -> IO a
withNetworkLayer tr (UseRunning cp) action = withNetworkLayerConn tr cp action
withNetworkLayer tr (Launch lj) action = withNetworkLayerLaunch tr lj action

withNetworkLayerLaunch
    :: forall a. ()
    => Trace IO Text
    -- ^ Logging of node startup.
    -> LaunchConfig
    -- ^ Configuration for starting J√∂rmungandr.
    -> (Either ErrStartup (ConnectionParams, NetworkLayer IO ByronBlock) -> IO a)
    -- ^ The action to run. It will be passed the connection parameters used,
    -- and a network layer if startup was successful.
    -> IO a
withNetworkLayerLaunch tr lj action = do
    withNode tr lj $ \cp -> withNetworkLayerConn tr cp action

mainnetConnectionParams :: ConnectionParams
mainnetConnectionParams =
    let
        -- I have cardano-node running from ../cardano-node
        nodeId = CoreNodeId 0
        path = "~/IOHK/cardano-node/socket/" <> (localSocketFilePath nodeId)
        addr =  localSocketAddrInfo path

        params = ChainParameters
             { epochSlots = EpochSlots 21600
             , protocolMagic = ProtocolMagicId 764824073
             }
        block0 = Hash "f0f7892b5c333cffc4b3c4344de48af4cc63f55e44936196f365a9ef2244134f"
        -- ^ is this correct?
    in
        ConnectionParams block0 params addr


withNode
    :: Trace IO Text
    -> LaunchConfig
    -> (ConnectionParams -> IO a)
    -> IO a
withNode tr launchConfig action = do
    let args = []
    let cmd = Command
            "../../../cardano-node/scripts/mainnet.sh"
            args
            (return ())
            (outputStream launchConfig)
    let tr' = transformTextTrace tr
    res <- withBackendProcess tr' cmd $ do
        action mainnetConnectionParams
    return $ either (error "startup failed (todo: proper error)") id res

withNetworkLayerConn
    :: forall a. ()
    => Trace IO Text
    -- ^ Logging of network layer startup
    -> ConnectionParams
    -- ^ Parameters for connecting to J√∂rmungandr node which is already running.
    -> (Either ErrStartup (ConnectionParams, NetworkLayer IO ByronBlock) -> IO a)
    -- ^ Action to run with the network layer.
    -> IO a
withNetworkLayerConn tr connectionParams action = do
    nw <- newNetworkLayer tr connectionParams
    action $ Right (connectionParams, nw)

-- | Hard-coded slot duration
byronSlotLength :: SlotLength
byronSlotLength = SlotLength 20

-- | Hard-coded max transaction size
byronTxMaxSize :: Quantity "byte" Word16
byronTxMaxSize = Quantity 8192

-- | Hard-coded fee policy for Cardano on Byron
byronFeePolicy :: FeePolicy
byronFeePolicy = LinearFee (Quantity 155381) (Quantity 43.946) (Quantity 0)

byronBlockchainParameters
     :: Network
     -> BlockchainParameters
byronBlockchainParameters n = BlockchainParameters
     { getGenesisBlockHash = Hash "genesis"
     , getGenesisBlockDate = case n of
         Mainnet -> StartTime $ posixSecondsToUTCTime 1506203091
         Testnet -> StartTime $ posixSecondsToUTCTime 1563999616
     , getFeePolicy = byronFeePolicy
     , getSlotLength = byronSlotLength
     , getTxMaxSize = byronTxMaxSize
     , getEpochLength = EpochLength 21600
     , getEpochStability = Quantity 2160
     , getActiveSlotCoefficient = ActiveSlotCoefficient 1
     }

-- | Creates a new 'NetworkLayer' connecting to an underlying 'Jormungandr'
-- backend target.
newNetworkLayer
    :: Trace IO Text
    -> ConnectionParams
    -> IO (NetworkLayer IO ByronBlock)
newNetworkLayer _tr (ConnectionParams _ params addr)= do
    -- TODO: Note we are discarding the tracer here
    let t = nullTracer
    return $ NetworkLayer
        { follow = \knownPoints action -> do
            print ("follow"::String)
            let client = OuroborosInitiatorApplication $ \pid -> \case
                    ChainSyncWithBlocksPtcl ->
                        chainSyncWithBlocks
                            pid t params
                            action
                            (map toPoint knownPoints)
                    LocalTxSubmissionPtcl ->
                        localTxSubmission pid t
            connectClient client dummyNodeToClientVersion addr
            print ("hi"::String)

        , networkTip =
            error "networkTip unimplemented"

        , postTx =
            error ""

        , staticBlockchainParameters =
            let block0 =
            (block0, byronBlockchainParameters Testnet)

        , stakeDistribution =
            error "stakeDistribution"
        , getAccountBalance =
            error "getAccountBalance not implemented in Byron. TODO: return 0?"
        }

  where
    toPoint :: BlockHeader -> Point ByronBlock
    toPoint (BlockHeader sid _ h _) =
        O.Point $ Point.block
            (convertSlot sid)
            (convertHash h)

    convertSlot :: SlotId -> O.SlotNo
    convertSlot = O.SlotNo . flatSlot (EpochLength 21600)

    convertHash (Hash bytes) =
          case Crypto.digestFromByteString bytes of
            Nothing     -> error "digestFromByteString failed"
            Just digest -> ByronHash $ CC.AbstractHash digest

--------------------------------------------------------------------------------
--
-- Concrete Types

type Tx = GenTx ByronBlock

data ChainParameters = ChainParameters
    { epochSlots :: EpochSlots
        -- ^ Number of slots per epoch.
    , protocolMagic :: ProtocolMagicId
        -- ^ Protocol magic (e.g. mainnet=764824073, testnet=1097911063)
    } deriving (Eq, Show)

-- | Type representing a network client running two mini-protocols to sync
-- from the chain and, submit transactions.
type NetworkClient m = OuroborosApplication
    'InitiatorApp
        -- ^ Initiator ~ Client (as opposed to Responder / Server)
    ConnectionId
        -- ^ An identifier for the peer: here, a local and remote socket.
    NodeToClientProtocols
        -- ^ Specifies which mini-protocols our client is talking.
        -- 'NodeToClientProtocols' allows for two mini-protocols:
        --  - Chain Sync
        --  - Tx submission
    m
        -- ^ Underlying monad we run in
    ByteString
        -- ^ Concrete representation for bytes string
    Void
        -- ^ -- Return type of a network client. Void indicates that the client
        -- never exits.
    Void
        -- ^ Irrelevant for 'InitiatorApplication'. Return type of 'Responder'
        -- application.


--------------------------------------------------------------------------------
--
-- Network Client

-- Connect a client to a network, see `newNetworkClient` to construct a network
-- client interface.
--
-- >>> connectClient (newNetworkClient t params) dummyNodeToClientVersion addr
connectClient
    :: forall vData. (vData ~ NodeToClientVersionData)
    => NetworkClient IO
    -> (vData, CodecCBORTerm Text vData)
    -> AddrInfo
    -> IO ()
connectClient client (vData, vCodec) addr = do
    let vDict = DictVersion vCodec
    let versions = simpleSingletonVersions NodeToClientV_1 vData vDict client
    let connectTo' = connectTo $ NetworkConnectTracers nullTracer nullTracer
    -- TODO(anviking): what are these traces?
    connectTo' versions Nothing addr `catch` handleMuxError
  where
    -- `connectTo` might rise an exception: we are the client and the protocols
    -- specify that only  client can lawfuly close a connection, but the other
    -- side might just disappear.
    --
    -- NOTE: This handler does nothing.
    handleMuxError :: MuxError -> IO ()
    handleMuxError = throwIO

-- | A dummy network magic for a local cluster. When connecting to mainnet or
-- testnet, this should match the underlying network's configuration.
dummyNodeToClientVersion
    :: (NodeToClientVersionData, CodecCBORTerm Text NodeToClientVersionData)
dummyNodeToClientVersion =
    ( NodeToClientVersionData { networkMagic = NetworkMagic 764824073 }
    , nodeToClientCodecCBORTerm
    )


-- | Client for the 'Chain Sync' mini-protocol.
--
-- A corresponding 'Channel' can be obtained using a `MuxInitiatorApplication`
-- constructor.
chainSyncWithBlocks
    :: forall m protocol peerId. (protocol ~ ChainSync ByronBlock (Tip ByronBlock))
    => (MonadThrow m, Show peerId, MonadST m)
    => peerId
        -- ^ An abstract peer identifier for 'runPeer'
    -> Tracer m String
        -- ^ Base tracer for the mini-protocols
    -> ChainParameters
        -- ^ Some chain parameters necessary to encode/decode Byron 'Block'
    -> (RollForwardOrBack ByronBlock -> m ())
        -- ^ Action to take when receiving a block or rolling back
    -> [Point ByronBlock]
        -- ^ Starting point(s)
    -> Channel m ByteString
        -- ^ A 'Channel' is a abstract communication instrument which
        -- transports serialized messages between peers (e.g. a unix
        -- socket).
    -> m Void
chainSyncWithBlocks pid t params action startPoints channel =
    runPeer trace codec pid channel (chainSyncClientPeer client)
  where
    trace :: Tracer m (TraceSendRecv protocol peerId DeserialiseFailure)
    trace = contramap show t

    codec :: Codec protocol DeserialiseFailure m ByteString
    codec = codecChainSync
        encodeByronBlock
        (decodeByronBlock (epochSlots params))
        (O.encodePoint encodeByronHeaderHash)
        (O.decodePoint decodeByronHeaderHash)
        (O.encodeTip encodeByronHeaderHash)
        (O.decodeTip decodeByronHeaderHash)


    {-| A peer has agency if it is expected to send the next message.

                                    Agency
     ---------------------------------------------------------------------------
     Client has agency                 | Idle
     Server has agency                 | Intersect, Next

      *-----------*
      | Intersect |◀══════════════════════════════╗
      *-----------*         FindIntersect         ║
            │                                     ║
            │                                *---------*                *------*
            │ Intersect.{Unchanged,Improved} |         |═══════════════▶| Done |
            └───────────────────────────────╼|         |     MsgDone    *------*
                                             |   Idle  |
         ╔═══════════════════════════════════|         |
         ║            RequestNext            |         |⇦ START
         ║                                   *---------*
         ▼                                        ╿
      *------*       Roll.{Backward,Forward}      │
      | Next |────────────────────────────────────┘
      *------*

    -}

    client :: ChainSyncClient ByronBlock (Tip ByronBlock) m Void
    client = ChainSyncClient clientStIdle
      where

        -- Find intersection between wallet and node chains.
        clientStIdle :: m (ClientStIdle ByronBlock (Tip ByronBlock) m Void)
        clientStIdle = do
                pure $ SendMsgFindIntersect startPoints $ ClientStIntersect
                    { recvMsgIntersectFound = \intersection _tip ->
                        ChainSyncClient $
                            clientStFetchingBlocks intersection
                    , recvMsgIntersectNotFound = \_tip ->
                        ChainSyncClient $ do
                            clientStIdle
                    }

        clientStFetchingBlocks
            :: Point ByronBlock
                -- ^ Starting point
            -> m (ClientStIdle ByronBlock (Tip ByronBlock) m Void)
        clientStFetchingBlocks start = pure $ SendMsgRequestNext
            (ClientStNext
                { recvMsgRollForward = \block _tip -> ChainSyncClient $ do
                    action $ RollForward block
                    clientStFetchingBlocks start
                , recvMsgRollBackward = \point _tip -> ChainSyncClient $ do
                    -- TODO: Where do we want this conversion?
                    let slotIdFromPoint (O.Point p) =
                            fromFlatSlot (EpochLength 21600)
                            . O.unSlotNo
                            . Point.blockPointSlot
                            . maybe (error "TODO: handle genesis case") id
                            . Point.withOriginToMaybe
                            $ p
                    action $ RollBackTo (slotIdFromPoint point)
                    clientStFetchingBlocks start
                }
            )
            (do
                -- NOTE
                -- In the 'Next' state, a client can receive either an immediate
                -- response (previous handler), or a response indicating that
                -- the blocks can't be retrieve _on the moment_ but will be
                -- after a while. In this case, we simply yield a response to
                -- our interface immediately and, discard whatever messages we
                -- eventually get back from the node once the block becomes
                -- available.
                pure $ ClientStNext
                    { recvMsgRollForward = \_block _tip ->
                        ChainSyncClient clientStIdle
                    , recvMsgRollBackward = \_point _tip ->
                        ChainSyncClient clientStIdle
                    }
            )

-- | Client for the 'Local Tx Submission' mini-protocol.
--
-- A corresponding 'Channel' can be obtained using a `MuxInitiatorApplication`
-- constructor.
localTxSubmission
    :: forall m protocol peerId. ()
    => (MonadThrow m, MonadTimer m, MonadST m, Show peerId)
    => (protocol ~ LocalTxSubmission Tx String)
    => peerId
        -- ^ An abstract peer identifier for 'runPeer'
    -> Tracer m String
        -- ^ Base tracer for the mini-protocols
    -> Channel m ByteString
        -- ^ A 'Channel' is a abstract communication instrument which
        -- transports serialized messages between peers (e.g. a unix
        -- socket).
    -> m Void
localTxSubmission pid t channel =
    runPeer trace codec pid channel (localTxSubmissionClientPeer client)
  where
    trace :: Tracer m (TraceSendRecv protocol peerId DeserialiseFailure)
    trace = contramap show t

    codec :: Codec protocol DeserialiseFailure m ByteString
    codec = codecLocalTxSubmission
        encodeByronGenTx -- Tx -> CBOR.Encoding
        decodeByronGenTx -- CBOR.Decoder s Tx
        CBOR.encode      -- String -> CBOR.Encoding
        CBOR.decode      -- CBOR.Decoder s String

    {-| A peer has agency if it is expected to send the next message.

                                    Agency
     ---------------------------------------------------------------------------
     Client has agency                 | Idle
     Server has agency                 | Busy

      *-----------*
      |    Busy   |◀══════════════════════════════╗
      *-----------*            SubmitTx           ║
         │     │                                  ║
         │     │                             *---------*                *------*
         │     │        AcceptTx             |         |═══════════════▶| Done |
         │     └────────────────────────────╼|         |     MsgDone    *------*
         │              RejectTx             |   Idle  |
         └──────────────────────────────────╼|         |
                                             |         |⇦ START
                                             *---------*

    -}
    client :: LocalTxSubmissionClient Tx String m Void
    client = localTxSubmissionClientNull


--------------------------------------------------------------------------------
--
-- Transport

localSocketAddrInfo :: FilePath -> AddrInfo
localSocketAddrInfo socketPath = AddrInfo
    { addrFlags = []
    , addrFamily = AF_UNIX
    , addrProtocol = Socket.defaultProtocol
    , addrAddress = SockAddrUnix socketPath
    , addrCanonName = Nothing
    , addrSocketType = Stream
    }

localSocketFilePath :: CoreNodeId -> FilePath
localSocketFilePath (CoreNodeId n) =
    "node-core-" ++ show n ++ ".socket"

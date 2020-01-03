{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Network
    (
    -- * Interface
      NetworkLayer (..)
    , RollForwardOrBack (..)

    -- * Errors
    , ErrNetworkUnavailable (..)
    , ErrNetworkTip (..)
    , ErrGetBlock (..)
    , ErrPostTx (..)
    , ErrGetAccountBalance (..)

    -- * Initialization
    , defaultRetryPolicy
    , waitForNetwork
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , BlockchainParameters (..)
    , ChimericAccount (..)
    , EpochNo
    , Hash (..)
    , PoolId (..)
    , SealedTx
    , SlotId
    )
import Control.Arrow
    ( first )
import Control.Exception
    ( Exception (..) )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Control.Retry
    ( RetryPolicyM, constantDelay, limitRetriesByCumulativeDelay, retrying )
import Data.Map
    ( Map )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word64 )
import GHC.Generics
    ( Generic )
import UnliftIO.Exception
    ( throwIO )

data RollForwardOrBack block
    = RollForward block
    | RollBackTo SlotId
    deriving (Show, Eq, Functor)

data NetworkLayer m block = NetworkLayer
    { follow
        :: [BlockHeader]
        -> (RollForwardOrBack block -> IO ())
        -> m ()
        -- ^ Stateful, infinite chain sync

    , networkTip
        :: ExceptT ErrNetworkTip m BlockHeader
        -- ^ Get the current network tip from the chain producer

    , postTx
        :: SealedTx -> ExceptT ErrPostTx m ()
        -- ^ Broadcast a transaction to the chain producer

    , staticBlockchainParameters
        :: (block, BlockchainParameters)
        -- ^ Get the genesis block and blockchain parameters.

    , stakeDistribution
        :: ExceptT ErrNetworkUnavailable m
            ( EpochNo
            , Map PoolId (Quantity "lovelace" Word64)
            )
    , getAccountBalance
        :: ChimericAccount
        -> ExceptT ErrGetAccountBalance m (Quantity "lovelace" Word64)
    }

instance Functor m => Functor (NetworkLayer m) where
    fmap f nl = nl
        { follow = \start step -> follow nl start (step . fmap f)
        , staticBlockchainParameters = first f $ staticBlockchainParameters nl
        }

{-------------------------------------------------------------------------------
                                  Errors
-------------------------------------------------------------------------------}

-- | Network is unavailable
data ErrNetworkUnavailable
    = ErrNetworkUnreachable Text
      -- ^ Cannot connect to network backend.
    | ErrNetworkInvalid Text
      -- ^ Network backend reports that the requested network is invalid.
    deriving (Generic, Show, Eq)

instance Exception ErrNetworkUnavailable

-- | Error while trying to get the network tip
data ErrNetworkTip
    = ErrNetworkTipNetworkUnreachable ErrNetworkUnavailable
    | ErrNetworkTipNotFound
    deriving (Generic, Show, Eq)

instance Exception ErrNetworkTip

-- | Error while trying to get one or more blocks
data ErrGetBlock
    = ErrGetBlockNetworkUnreachable ErrNetworkUnavailable
    | ErrGetBlockNotFound (Hash "BlockHeader")
    deriving (Generic, Show, Eq)

-- | Error while trying to send a transaction
data ErrPostTx
    = ErrPostTxNetworkUnreachable ErrNetworkUnavailable
    | ErrPostTxBadRequest Text
    | ErrPostTxProtocolFailure Text
    deriving (Generic, Show, Eq)

instance Exception ErrPostTx

data ErrGetAccountBalance
    = ErrGetAccountBalanceNetworkUnreachable ErrNetworkUnavailable
    | ErrGetAccountBalanceAccountNotFound ChimericAccount
    deriving (Generic, Eq, Show)

{-------------------------------------------------------------------------------
                              Initialization
-------------------------------------------------------------------------------}

-- | Wait until 'networkTip networkLayer' succeeds according to a given
-- retry policy. Throws an exception otherwise.
waitForNetwork
    :: ExceptT ErrNetworkUnavailable IO ()
    -> RetryPolicyM IO
    -> IO ()
waitForNetwork getStatus policy = do
    r <- retrying policy shouldRetry (const $ runExceptT getStatus)
    case r of
        Right _ -> return ()
        Left e -> throwIO e
  where
    shouldRetry _ = \case
        Right _ ->
            return False
        Left ErrNetworkInvalid{} ->
            return False
        Left ErrNetworkUnreachable{} ->
            return True

-- | A default 'RetryPolicy' with a delay that starts short, and that retries
-- for no longer than a minute.
defaultRetryPolicy :: Monad m => RetryPolicyM m
defaultRetryPolicy =
    limitRetriesByCumulativeDelay (3600 * second) (constantDelay second)
  where
    second = 1000*1000

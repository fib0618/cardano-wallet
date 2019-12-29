{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Shelley.Transaction
    ( newTransactionLayer
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth, WalletKey (..), toChimericAccount )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Types
    ( Hash (..), SealedTx (..), Tx (..), TxOut (..) )
import Cardano.Wallet.Transaction
    ( ErrDecodeSignedTx (..)
    , ErrMkTx (..)
    , ErrValidateSelection
    , TransactionLayer (..)
    )
import Control.Arrow
    ( first, second )
import Control.Monad
    ( forM, when )
import Data.Either.Combinators
    ( maybeToRight )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( toText )
import Fmt
    ( Buildable (..) )

import qualified Data.ByteString.Lazy as BL

-- | Construct a 'TransactionLayer' compatible with Shelley and 'Jörmungandr'
newTransactionLayer
    :: forall k t. ( WalletKey k)
    => Hash "Genesis"
    -> TransactionLayer t k
newTransactionLayer block0H = TransactionLayer
    { mkStdTx = error "mkStdTx to be implemented"
    , mkDelegationJoinTx = error "mkDelegationJoinTx to be implemented"
    , mkDelegationQuitTx = error "mkDelegationQuitTx to be implemented"
    , decodeSignedTx = error "decodeSignedTx to be implemented"
    , estimateSize = error "estimateSize to be implemented"
    , estimateMaxNumberOfInputs = error "estimateMaxNumberOfInputs to be implemented"
    , validateSelection = error "validateSelection to be implemented"
    }

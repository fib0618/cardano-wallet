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

import Cardano.Wallet.Primitive.Types
    ( Hash (..) )
import Cardano.Wallet.Transaction
    ( TransactionLayer (..) )

-- | Construct a 'TransactionLayer' compatible with Shelley and 'Jörmungandr'
newTransactionLayer
    :: forall k t. ()
    => Hash "Genesis"
    -> TransactionLayer t k
newTransactionLayer _block0H = TransactionLayer
    { mkStdTx = error "mkStdTx to be implemented"
    , mkDelegationJoinTx = error "mkDelegationJoinTx to be implemented"
    , mkDelegationQuitTx = error "mkDelegationQuitTx to be implemented"
    , decodeSignedTx = error "decodeSignedTx to be implemented"
    , estimateSize = error "estimateSize to be implemented"
    , estimateMaxNumberOfInputs = error "estimateMaxNumberOfInputs to be implemented"
    , validateSelection = error "validateSelection to be implemented"
    }

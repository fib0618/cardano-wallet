{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.ByronWallets2
    ( spec
    ) where

import Prelude

import Test.Hspec
    ( SpecWith, describe, it, runIO )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , expectFieldSatisfy
    , expectResponseCode
    , fixtureLegacyWallet
    )

import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t n. (n ~ 'Testnet) => SpecWith (Context t)
spec = describe "PATATE" undefined

byronCalculate01
    :: forall (style :: Symbol) t. ()
    => Context t
    -> Spec
byronCalculate01 ctx = do
    w <- fixtureLegacyWallet @style ctx
    verify r
        [ expectResponseCode @IO HTTP.status200
        , expectFieldSatisfy amount (> 0)
        ]

calculateByronMigrationCost
    :: HasType WalletId s
    => s
    -> Context t
    -> IO ApiByronWalletMigrationInfo
calculateByronMigrationCost s =
    request ctx (calculateByronMigrationCostEp s) Default Empty

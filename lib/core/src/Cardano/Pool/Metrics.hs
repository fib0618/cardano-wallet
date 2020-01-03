{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module can fold over a blockchain to collect metrics about
-- Stake pools.
--
-- It interacts with:
-- - "Cardano.Wallet.Network" which provides the chain
-- - "Cardano.Pool.DB" - which can persist the metrics
-- - "Cardano.Wallet.Api.Server" - which presents the results in an endpoint
module Cardano.Pool.Metrics
    ( -- * Types
      Block (..)
    , StakePool (..)

    -- * Listing stake-pools from the DB
    , StakePoolLayer (..)
    , newStakePoolLayer
    , ErrListStakePools (..)

      -- * Following the chain
    , monitorStakePools

      -- * Combining Metrics
    , ErrMetricsInconsistency (..)
    , combineMetrics
    , calculatePerformance

      -- * Associating metadata
    , associateMetadata

      -- * Logging
    , StakePoolLayerMsg (..)
    )
    where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( DefinePrivacyAnnotation (..), DefineSeverity (..) )
import Cardano.BM.Trace
    ( Trace )
import Cardano.Pool.DB
    ( DBLayer (..), ErrPointAlreadyExists )
import Cardano.Pool.Metadata
    ( RegistryLog
    , StakePoolMetadata (..)
    , getMetadataConfig
    , getStakePoolMetadata
    , sameStakePoolMetadata
    )
import Cardano.Wallet.Logging
    ( logTrace )
import Cardano.Wallet.Network
    ( ErrNetworkTip
    , ErrNetworkUnavailable
    , NetworkLayer (networkTip)
    , staticBlockchainParameters
    )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , BlockHeader (..)
    , EpochLength (..)
    , EpochNo (..)
    , PoolId (..)
    , PoolOwner (..)
    , PoolRegistrationCertificate (..)
    , SlotId (..)
    , SlotNo (unSlotNo)
    )
import Control.Arrow
    ( first )
import Control.Monad
    ( forM, when )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), throwE, withExceptT )
import Control.Tracer
    ( contramap )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.List
    ( foldl', nub, nubBy, sortOn, (\\) )
import Data.Map.Merge.Strict
    ( dropMissing, traverseMissing, zipWithMatched )
import Data.Map.Strict
    ( Map )
import Data.Ord
    ( Down (..) )
import Data.Quantity
    ( Percentage, Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Vector.Shuffle
    ( shuffleWith )
import Data.Word
    ( Word64 )
import Fmt
    ( pretty )
import GHC.Generics
    ( Generic )
import System.Random
    ( StdGen )

import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Information from a block relevant to monitoring stake pools.
data Block = Block
    { header :: BlockHeader
    , producer :: PoolId
    -- ^ The stake pool that minted this block.
    , poolRegistrations :: ![PoolRegistrationCertificate]
    -- ^ Any stake pools that were registered in this block.
    } deriving (Eq, Show, Generic)

data StakePool = StakePool
    { poolId :: PoolId
    , stake :: Quantity "lovelace" Word64
    , production :: Quantity "block" Word64
    , apparentPerformance :: Double
    , cost :: Quantity "lovelace" Word64
    , margin :: Percentage
    } deriving (Show, Generic)

--------------------------------------------------------------------------------
-- Stake Pool Monitoring
--------------------------------------------------------------------------------

-- | 'monitorStakePools' follows the chain and puts pool productions and stake
-- distributions to a 'DBLayer', such that the data in the database is always
-- consistent.
--
-- The pool productions and stake distrubtions in the db can /never/ be from
-- different forks such that it's safe for readers to access it.
monitorStakePools
    :: Trace IO Text
    -> NetworkLayer IO Block
    -> DBLayer IO
    -> IO ()
monitorStakePools _tr _nl _db = do
    -- TODO:
    -- monitorStakePools was designed for jormungandr. We need to
    -- generalize it.
    return ()

-- | Internal error data-type used to drive the 'forward' logic
data ErrMonitorStakePools
    = ErrMonitorStakePoolsNetworkUnavailable ErrNetworkUnavailable
    | ErrMonitorStakePoolsPoolAlreadyExists ErrPointAlreadyExists
    | ErrMonitorStakePoolsNetworkTip ErrNetworkTip
    | ErrMonitorStakePoolsWrongTip
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- StakePoolLayer
--------------------------------------------------------------------------------

-- | @StakePoolLayer@ is a thin layer ontop of the DB. It is /one/ value that
-- can easily be passed to the API-server, where it can be used in a simple way.
data StakePoolLayer m = StakePoolLayer
    { listStakePools
        :: ExceptT ErrListStakePools m [(StakePool, Maybe StakePoolMetadata)]

    , knownStakePools
        :: m [PoolId]
        -- ^ Get a list of known pools that doesn't require fetching things from
        -- Jörmungandr or any registry. This list comes from the registration
        -- certificates that have been seen on chain.
    }

data ErrListStakePools
     = ErrMetricsIsUnsynced (Quantity "percent" Percentage)
     | ErrListStakePoolsMetricsInconsistency ErrMetricsInconsistency
     | ErrListStakePoolsErrNetworkTip ErrNetworkTip
     deriving (Show)

newStakePoolLayer
    :: Trace IO StakePoolLayerMsg
    -> DBLayer IO
    -> NetworkLayer IO Block
    -> FilePath
    -- ^ A directory to cache downloaded stake pool metadata. Will be created if
    -- it does not exist.
    -> StakePoolLayer IO
newStakePoolLayer tr db@DBLayer{..} nl metadataDir = StakePoolLayer
    { listStakePools = do
        lift $ logTrace tr MsgListStakePoolsBegin
        stakePools <- sortKnownPools
        meta <- lift $ findMetadata (map (first (^. #poolId)) stakePools)
        pure $ zip (map fst stakePools) meta
    , knownStakePools =
        atomically listRegisteredPools
    }
  where
    sortKnownPools :: ExceptT ErrListStakePools IO [(StakePool, [PoolOwner])]
    sortKnownPools = do
        nodeTip <- withExceptT ErrListStakePoolsErrNetworkTip
            $ networkTip nl
        let nodeEpoch = nodeTip ^. #slotId . #epochNumber
        let genesisEpoch = block0 ^. #header . #slotId . #epochNumber

        (distr, prod, prodTip) <- liftIO . atomically $ (,,)
            <$> (Map.fromList <$> readStakeDistribution nodeEpoch)
            <*> readPoolProduction nodeEpoch
            <*> readPoolProductionTip


        when (Map.null distr || Map.null prod) $ do
            liftIO $ logTrace tr $ MsgComputedProgress prodTip nodeTip
            throwE $ ErrMetricsIsUnsynced $ computeProgress prodTip nodeTip

        if nodeEpoch == genesisEpoch
        then do
            seed <- liftIO $ atomically readSystemSeed
            combineWith (sortArbitrarily seed) distr (count prod) mempty

        else do
            let sl = prodTip ^. #slotId
            perfs <- liftIO $ readPoolsPerformances db activeSlotCoeff epochLength sl
            combineWith (pure . sortByPerformance) distr (count prod) perfs

    readPoolProductionTip = readPoolProductionCursor 1 <&> \case
        []  -> header block0
        h:_ -> h

    -- For each pool, look up its metadata. If metadata could not be found for a
    -- pool, the result will be 'Nothing'.
    findMetadata :: [(PoolId, [PoolOwner])] -> IO [Maybe StakePoolMetadata]
    findMetadata pools = do
        -- note: this will become simpler once we cache metadata in the database
        let (poolIds, owners) = unzip pools
        let owners' = nub $ concat owners
        cfg <- getMetadataConfig metadataDir
        let tr' = contramap (fmap MsgRegistry) tr
        getStakePoolMetadata tr' cfg owners' >>= \case
            Left _ -> do
                logTrace tr MsgMetadataUnavailable
                pure $ replicate (length poolIds) Nothing
            Right metas -> do
                let res = associateMetadata pools (zip owners' metas)
                mapM_ (logTrace tr . fst) res
                pure $ map snd res

    (block0, bp) = staticBlockchainParameters nl
    epochLength = bp ^. #getEpochLength
    activeSlotCoeff = bp ^. #getActiveSlotCoefficient

    combineWith
        :: ([(StakePool, [PoolOwner])] -> IO [(StakePool, [PoolOwner])])
        -> Map PoolId (Quantity "lovelace" Word64)
        -> Map PoolId (Quantity "block" Word64)
        -> Map PoolId Double
        -> ExceptT ErrListStakePools IO [(StakePool, [PoolOwner])]
    combineWith sortResults distr prod perfs = do
        case combineMetrics distr prod perfs of
            Left e ->
                throwE $ ErrListStakePoolsMetricsInconsistency e
            Right ps -> lift $ do
                let len = fromIntegral (length ps)
                let avg = if null ps
                        then 0
                        else sum ((\(_,_,c) -> c) <$> (Map.elems ps)) / len
                ns <- readNewcomers db (Map.keys ps) avg
                pools <- atomically $
                    Map.traverseMaybeWithKey mergeRegistration (ps <> ns)
                sortResults $ Map.elems pools
      where
        mergeRegistration poolId (stake, production, apparentPerformance) =
            fmap mkStakePool <$> readPoolRegistration poolId
          where
            mkStakePool PoolRegistrationCertificate{poolCost,poolMargin,poolOwners} =
                ( StakePool
                    { poolId
                    , stake
                    , production
                    , apparentPerformance
                    , cost = poolCost
                    , margin = poolMargin
                    }
                , poolOwners
                )

    sortByPerformance :: [(StakePool, a)] -> [(StakePool, a)]
    sortByPerformance = sortOn (Down . apparentPerformance . fst)

    sortArbitrarily :: StdGen -> [a] -> IO [a]
    sortArbitrarily = shuffleWith

    computeProgress
        :: BlockHeader -- ^ ... / denominator
        -> BlockHeader -- ^ numerator /...
        -> Quantity "percent" Percentage
    computeProgress prodTip nodeTip =
        Quantity $ if s1 == 0
            then minBound
            else toEnum $ round $ 100 * (toD s0) / (toD s1)
      where
        s0 = getQuantity $ prodTip ^. #blockHeight
        s1 = getQuantity $ nodeTip ^. #blockHeight
        toD :: Integral i => i -> Double
        toD = fromIntegral

readNewcomers
    :: Monad m
    => DBLayer m
    -> [PoolId]
    -> Double
    -> m (Map PoolId (Quantity "lovelace" Word64, Quantity "block" Word64, Double))
readNewcomers DBLayer{..} elders avg = do
    pids <- atomically listRegisteredPools
    pure $ Map.fromList $ zip
        (pids \\ elders)
        (repeat (Quantity 0, Quantity 0, avg))

readPoolsPerformances
    :: DBLayer m
    -> ActiveSlotCoefficient
    -> EpochLength
    -> SlotId
    -> m (Map PoolId Double)
readPoolsPerformances DBLayer{..} activeSlotCoeff (EpochLength el) tip = do
    atomically $ fmap avg $ forM historicalEpochs $ \ep -> calculatePerformance
        activeSlotCoeff
        (slotsInEpoch ep)
        <$> (Map.fromList <$> readStakeDistribution ep)
        <*> (count <$> readPoolProduction ep)
  where
    currentEpoch = tip ^. #epochNumber

    historicalEpochs :: [EpochNo]
    historicalEpochs
        | currentEpoch > window = [currentEpoch - window .. currentEpoch]
        | otherwise             = [0..currentEpoch]
      where
        window = 14

    slotsInEpoch :: EpochNo -> Int
    slotsInEpoch e =
        if e == currentEpoch
        then fromIntegral $ unSlotNo $ tip ^. #slotNumber
        else fromIntegral el

    -- | Performances are computed over many epochs to cope with the fact that
    -- our data is sparse (regarding stake distribution at least).
    --
    -- So the approach is the following:
    --
    -- 1. Compute performances, if available, for the last `n` epochs
    -- 2. Compute the average performance for all epochs for which we had data
    avg :: [Map PoolId Double] -> Map PoolId Double
    avg performances =
        Map.map (/ len) . Map.unionsWith (+) $ performances
      where
        len = fromIntegral $ length $ filter (not . Map.null) performances

-- | Calculate pool apparent performance over the given data. The performance
-- is a 'Double' between 0 and 1 as:
--
-- @
--     p = n / (f * N) * S / s
--   where
--     n = number of blocks produced in an epoch e
--     N = number of slots in e
--     f = active slot coeff, i.e. % of slots for which a leader can be elected.
--     s = stake owned by the pool in e
--     S = total stake delegated to pools in e
-- @
--
-- Note that, this apparent performance is clamped to [0,1] as it may in
-- practice, be greater than 1 if a stake pool produces more than it is
-- expected.
calculatePerformance
    :: ActiveSlotCoefficient
    -> Int
    -> Map PoolId (Quantity "lovelace" Word64)
    -> Map PoolId (Quantity "block" Word64)
    -> Map PoolId Double
calculatePerformance (ActiveSlotCoefficient f) nTotal mStake mProd =
    let
        stakeButNotProd = traverseMissing $ \_ _ -> 0
        prodButNoStake  = dropMissing
        stakeAndProd sTotal = zipWithMatched $ \_ s n ->
            if (nTotal == 0 || s == Quantity 0) then
                0
            else
                min 1 ((double n / (f * fromIntegral nTotal)) * (sTotal / double s))
    in
        Map.merge
            stakeButNotProd
            prodButNoStake
            (stakeAndProd (sumQ mStake))
            mStake
            mProd
  where
    double :: Integral a => Quantity any a -> Double
    double (Quantity a) = fromIntegral a

    sumQ :: Integral a => Map k (Quantity any a) -> Double
    sumQ = fromIntegral . foldl' (\y (Quantity x) -> (y + x)) 0 . Map.elems

-- | Combines three different sources of data into one:
--
-- 1. A stake-distribution map
-- 2. A pool-production map
-- 3. A pool-performance map
--
-- If a pool has produced a block without existing in the stake-distribution,
-- i.e it exists in (2) but not (1), this function will return
-- @Left ErrMetricsInconsistency@.
--
-- If a pool is in (1) but not (2), it simply means it has produced 0 blocks so
-- far.
--
-- Similarly, if we do have metrics about a pool in (3), but this pool is
-- unknown from (1) & (2), this function also returns
-- @Left ErrMetricsInconsistency@.
--
-- If a pool is in (1+2) but not in (3), it simply means it has produced 0
-- blocks so far.
combineMetrics
    :: Map PoolId (Quantity "lovelace" Word64)
    -> Map PoolId (Quantity "block" Word64)
    -> Map PoolId Double
    -> Either
        ErrMetricsInconsistency
        ( Map PoolId
            ( Quantity "lovelace" Word64
            , Quantity "block" Word64
            , Double
            )
        )
combineMetrics mStake mProd mPerf = do
    let errMissingLeft = ErrProducerNotInDistribution
    mActivity <- zipWithRightDefault (,) errMissingLeft (Quantity 0) mStake mProd
    zipWithRightDefault unzipZip3 errMissingLeft 0 mActivity mPerf
  where
    unzipZip3 :: (a,b) -> c -> (a,b,c)
    unzipZip3 (a,b) c = (a,b,c)

-- | Possible errors returned by 'combineMetrics'.
newtype ErrMetricsInconsistency
    = ErrProducerNotInDistribution PoolId
        -- ^ Somehow, we tried to combine invalid metrics together and passed
        -- a passed a block production that doesn't match the producers found in
        -- the stake activity.
        --
        -- Note that the opposite case is okay as we only observe pools that
        -- have produced blocks. So it could be the case that a pool exists in
        -- the distribution but not in the production! (In which case, we'll
        -- assign it a production of '0').
    deriving (Show, Eq)

-- | Combine two maps with the given zipping function. It defaults when elements
-- of the first map (left) are not present in the second (right), but returns an
-- error when elements of the second (right) map are not present in the first
-- (left).
--
-- Example:
--
-- @
-- let m1 = Map.fromList [(1, 'a'), (2, 'b'), (3, 'c')]
-- let m2 = Map.fromList [(2, True)]
-- @
--
-- >>> zipWithRightDefault (,) ErrMissing False m1 m2
-- Right (Map.fromList [(1, ('a', False)), (2, ('b', True)), (3, ('c', False))])
--
-- >>> zipWithRightDefault (,) ErrMissing False m2 m1
-- Left (ErrMissing 1)
zipWithRightDefault
    :: Ord k
    => (l -> r -> a)
    -> (k -> errMissingLeft)
    -> r
    -> Map k l
    -> Map k r
    -> Either errMissingLeft (Map k a)
zipWithRightDefault combine onMissing rZero =
    Map.mergeA leftButNotRight rightButNotLeft bothPresent
  where
    leftButNotRight = traverseMissing $ \_k l -> pure (combine l rZero)
    rightButNotLeft = traverseMissing $ \k _r -> Left (onMissing k)
    bothPresent     = zipWithMatched  $ \_k l r -> (combine l r)

-- | Count elements inside a 'Map'
count :: Map k [a] -> Map k (Quantity any Word64)
count = Map.map (Quantity . fromIntegral . length)

-- | Given a mapping from 'PoolId' -> 'PoolOwner' and a mapping between
-- 'PoolOwner' <-> 'StakePoolMetadata', return a matching 'StakePoolMeta' entry
-- for every 'PoolId'.
--
-- If there is no metadata for a pool, it returns Nothing for that 'PoolId'.
-- If there is different metadata submitted by multiple owners of a pool, it returns Nothing.
-- If there is one unique metadata for a pool, it returns 'Just' the metadata for that 'PoolId'.
--
-- It also provides a log message for each association.
associateMetadata
    :: [(PoolId, [PoolOwner])]
    -- ^ Ordered mapping from pool to owner(s).
    -> [(PoolOwner, Maybe StakePoolMetadata)]
    -- ^ Association between owner and metadata
    -> [(StakePoolLayerMsg, Maybe StakePoolMetadata)]
associateMetadata poolOwners ownerMeta =
    map (uncurry getResult . fmap associate) poolOwners
  where
    -- Filter the metadata to just the entries which were submitted by the given
    -- owners.
    associate :: [PoolOwner] -> [(PoolOwner, StakePoolMetadata)]
    associate owners = [(a, b) | (a, Just b) <- ownerMeta, a `elem` owners]

    -- Ensure that there is exactly one unique metadata per pool.
    -- Produces a log message and validated result.
    getResult
        :: PoolId
        -> [(PoolOwner, StakePoolMetadata)]
        -> (StakePoolLayerMsg, Maybe StakePoolMetadata)
    getResult pid metas = case nubBy sameMeta metas of
        [(owner, meta)] -> (MsgMetadataUsing pid owner meta, Just meta)
        [] -> (MsgMetadataMissing pid, Nothing)
        metas' -> (MsgMetadataMultiple pid metas', Nothing)

    sameMeta (_, a) (_, b) = sameStakePoolMetadata a b

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data StakePoolLayerMsg
    = MsgRegistry RegistryLog
    | MsgListStakePoolsBegin
    | MsgMetadataUnavailable
    | MsgMetadataUsing PoolId PoolOwner StakePoolMetadata
    | MsgMetadataMissing PoolId
    | MsgMetadataMultiple PoolId [(PoolOwner, StakePoolMetadata)]
    | MsgComputedProgress BlockHeader BlockHeader
    deriving (Show, Eq)

instance DefinePrivacyAnnotation StakePoolLayerMsg
instance DefineSeverity StakePoolLayerMsg where
    defineSeverity ev = case ev of
        MsgRegistry msg -> defineSeverity msg
        MsgListStakePoolsBegin -> Debug
        MsgMetadataUnavailable -> Notice
        MsgComputedProgress{} -> Debug
        MsgMetadataUsing{} -> Debug
        MsgMetadataMissing{} -> Debug
        MsgMetadataMultiple{} -> Debug

instance ToText StakePoolLayerMsg where
    toText = \case
        MsgRegistry msg -> toText msg
        MsgListStakePoolsBegin -> "Listing stake pools"
        MsgMetadataUnavailable -> "Stake pool metadata is unavailable"
        MsgComputedProgress prodTip nodeTip -> mconcat
            [ "The node tip is:\n"
            , pretty nodeTip
            , ",\nbut the last pool production stored in the db"
            , " is from:\n"
            , pretty prodTip
            ]
        MsgMetadataUsing pid owner _ ->
            "Using stake pool metadata from " <>
            toText owner <> " for " <> toText pid
        MsgMetadataMissing pid ->
            "No stake pool metadata for " <> toText pid
        MsgMetadataMultiple pid _ ->
            "Multiple different metadata registered for " <> toText pid

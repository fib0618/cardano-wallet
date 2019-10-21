{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

module Cardano.Pool.DB.Properties
    ( properties
    , withDB
    , newMemoryDBLayer
    ) where

import Prelude

import Cardano.BM.Configuration.Model
    ( Configuration )
import Cardano.BM.Data.LogItem
    ( LogObject (..) )
import Cardano.BM.Trace
    ( traceInTVarIO )
import Cardano.DB.Sqlite
    ( SqliteContext )
import Cardano.Pool.DB
    ( DBLayer (..), ErrPointAlreadyExists (..) )
import Cardano.Pool.DB.Arbitrary
    ( StakePoolsFixture (..) )
import Cardano.Pool.DB.Sqlite
    ( newDBLayer )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), EpochNo, PoolId, SlotId (..) )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Monad
    ( forM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.Function
    ( on )
import Data.Functor
    ( ($>) )
import Data.List.Extra
    ( nubOrd )
import Data.Map.Strict
    ( Map )
import Data.Ord
    ( Down (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word64 )
import Fmt
    ( pretty )
import GHC.Conc
    ( TVar, newTVarIO )
import Test.Hspec
    ( Expectation
    , Spec
    , SpecWith
    , beforeAll
    , beforeWith
    , describe
    , it
    , shouldBe
    , shouldReturn
    )
import Test.QuickCheck
    ( Property, classify, counterexample, property )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )

import qualified Cardano.Pool.DB.MVar as MVar
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Provide a DBLayer to a Spec that requires it. The database is initialised
-- once, and cleared with 'cleanDB' before each test.
withDB :: IO (DBLayer IO) -> SpecWith (DBLayer IO) -> Spec
withDB create = beforeAll create . beforeWith (\db -> cleanDB db $> db)

-- | Set up a DBLayer for testing, with the command context, and the logging
-- variable.
newMemoryDBLayer :: IO Configuration -> IO (DBLayer IO)
newMemoryDBLayer conf = snd . snd <$> (newMemoryDBLayer' conf)

newMemoryDBLayer'
    :: IO Configuration -> IO (TVar [LogObject Text], (SqliteContext, DBLayer IO))
newMemoryDBLayer' testingLogConfig = do
    logConfig <- testingLogConfig
    logVar <- newTVarIO []
    (logVar, ) <$> newDBLayer logConfig (traceInTVarIO logVar) Nothing

properties :: SpecWith (DBLayer IO)
properties = do
    describe "Stake Pool properties" $ do
        it "putPoolProduction . readPoolProduction yields expected results"
            (property . prop_putReadPoolProduction)
        it "putPoolProduction with already put slot yields error"
            (property . prop_putSlotTwicePoolProduction)
        it "Rollback of stake pool production"
            (property . prop_rollbackPools)
        it "readPoolProduction for a given epoch should always give slots \
           \from given epoch"
            (property . prop_readPoolNoEpochLeaks)
        it "readPoolProduction should never give pools with no slots"
            (property . (prop_readPoolCond noEmptyPools))
        it "readPoolProduction should never give pools with no slots \
           \after consecutive 1-slot-depth rollbacks"
            (property . (prop_readPoolCondAfterDeterministicRollbacks noEmptyPools))
        it "readPoolProduction should never give pools with no slots \
           \after rollback - arbitrary N-slot-depth rollbacks"
            (property . (prop_readPoolCondAfterRandomRollbacks noEmptyPools))
        it "readPoolProduction should give pools with descending slots"
            (property . (prop_readPoolCond descSlotsPerPool))
        it "readPoolProduction should give pools with descending slots \
           \after consecutive 1-slot-depth rollbacks"
            (property . (prop_readPoolCondAfterDeterministicRollbacks descSlotsPerPool))
        it "readPoolProduction should never give pools with no slots \
           \after rollback - arbitrary N-slot-depth rollbacks"
            (property . (prop_readPoolCondAfterRandomRollbacks descSlotsPerPool))
        it "readStakeDistribution . putStakeDistribution == pure"
            (property . prop_putStakeReadStake)
        it "readStake . putStake a1 . putStake s0 == pure a1"
            (property . prop_putStakePutStake)

{-------------------------------------------------------------------------------
                                    Properties
-------------------------------------------------------------------------------}

-- | Can read put pool production
prop_putReadPoolProduction
    :: DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_putReadPoolProduction db (StakePoolsFixture pairs _) =
    monadicIO (setup >>= prop)
  where
    setup = liftIO $ do
        cleanDB db
        db' <- MVar.newDBLayer
        cleanDB db'
        pure db'
    prop db' = liftIO $ do
        forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction db slot pool
        forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction db' slot pool
        forM_ (uniqueEpochs pairs) $ \epoch -> do
            res' <- readPoolProduction db' epoch
            readPoolProduction db epoch `shouldReturn` res'

-- | Cannot put pool production with already put slot
prop_putSlotTwicePoolProduction
    :: DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_putSlotTwicePoolProduction db (StakePoolsFixture pairs _) =
    monadicIO (setup >> prop)
  where
    setup = liftIO $ cleanDB db
    prop = liftIO $ do
        forM_ pairs $ \(pool, slot) -> do
            let err = ErrPointAlreadyExists slot
            runExceptT (putPoolProduction db slot pool) `shouldReturn` Right ()
            runExceptT (putPoolProduction db slot pool) `shouldReturn` Left err

-- | Rolling back wipes out pool production statistics after the rollback point.
prop_rollbackPools
    :: DBLayer IO
    -> StakePoolsFixture
    -> SlotId
    -> Property
prop_rollbackPools db f@(StakePoolsFixture pairs _) sl =
    monadicIO prop
  where
    prop = do
        (beforeRollback, afterRollback) <- run $ do
            forM_ pairs $ \(pool, point) ->
                runExceptT $ putPoolProduction db point pool
            before <- map fst <$> allPoolProduction db f
            rollbackTo db sl
            after <- map fst <$> allPoolProduction db f
            pure (before, after)

        monitor $ counterexample $ unlines
            [ "Rollback point:    " <> showSlot sl
            , "Production before: " <> unwords (map showSlot beforeRollback)
            , "Production after:  " <> unwords (map showSlot afterRollback)
            ]
        monitor $ classify (any (> sl) beforeRollback) "something to roll back"
        monitor $ classify (all (<= sl) beforeRollback) "nothing to roll back"

        assert $ all (<= sl) afterRollback

    showSlot s = T.unpack $ pretty s

-- | Can read pool production only for a given epoch
prop_readPoolNoEpochLeaks
    :: DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_readPoolNoEpochLeaks db (StakePoolsFixture pairs _) =
    monadicIO (setup >> prop)
  where
    slotPartition = L.groupBy ((==) `on` epochNumber)
        $ L.sortOn epochNumber
        $ map (slotId . snd) pairs
    epochGroups = L.zip (uniqueEpochs pairs) slotPartition
    setup = liftIO $ cleanDB db
    prop = liftIO $ do
        forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction db slot pool
        forM_ epochGroups $ \(epoch, slots) -> do
            slots' <- (Set.fromList . map slotId . concat . Map.elems) <$>
                readPoolProduction db epoch
            slots' `shouldBe` (Set.fromList slots)

-- | Read pool production satisfies conditions after consecutive
-- 1-slot-depth rollbacks
prop_readPoolCondAfterDeterministicRollbacks
    :: (Map PoolId [BlockHeader] -> Expectation)
    -> DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_readPoolCondAfterDeterministicRollbacks cond db (StakePoolsFixture pairs _) =
    monadicIO (setup >> prop)
  where
    setup = liftIO $ cleanDB db
    slots = map (slotId . snd) pairs
    prop = liftIO $ do
        forM_ pairs $ \(pool, point) ->
            unsafeRunExceptT $ putPoolProduction db point pool
        forM_ slots $ \slot -> do
            _ <- rollbackTo db slot
            forM_ (uniqueEpochs pairs) $ \epoch -> do
                res <- readPoolProduction db epoch
                cond res

-- | Read pool production satisfies conditions after consecutive
-- arbitrary N-slot-depth rollbacks
prop_readPoolCondAfterRandomRollbacks
    :: (Map PoolId [BlockHeader] -> Expectation)
    -> DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_readPoolCondAfterRandomRollbacks cond db
    (StakePoolsFixture pairs rSlots) =
    monadicIO (setup >> prop)
  where
    setup = liftIO $ cleanDB db
    prop = do
        run $ forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction db slot pool
        run $ forM_ rSlots $ \slot -> do
            rollbackTo db slot
            forM_ (uniqueEpochs pairs) $ \epoch -> do
                res <- readPoolProduction db epoch
                cond res
        monitor $ classify (length pairs <= 10) "number of slots <= 10"
        monitor $ classify (length pairs > 10) "number of slots > 10"

-- | Read pool production satisfies condition
prop_readPoolCond
    :: (Map PoolId [BlockHeader] -> Expectation)
    -> DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_readPoolCond cond db (StakePoolsFixture pairs _) =
    monadicIO (setup >> prop)
  where
    setup = liftIO $ cleanDB db
    prop = liftIO $ do
        forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction db slot pool
        forM_ (uniqueEpochs pairs) $ \epoch -> do
            res <- readPoolProduction db epoch
            cond res

-- | read . put == pure
prop_putStakeReadStake
    :: DBLayer IO
    -> EpochNo
    -> [(PoolId, Quantity "lovelace" Word64)]
    -> Property
prop_putStakeReadStake db epoch distribution =
    monadicIO (setup >> prop)
  where
    setup = run (cleanDB db)
    prop = do
        run $ putStakeDistribution db epoch distribution
        distribution' <- run $ readStakeDistribution db epoch
        monitor $ counterexample $ unlines
            [ "Read from DB: " <> show distribution' ]
        monitor $ classify (null distribution) "Empty distributions"
        assert (L.sort distribution' == L.sort distribution)

-- | read $ put B $ put A == B
prop_putStakePutStake
    :: DBLayer IO
    -> EpochNo
    -> [(PoolId, Quantity "lovelace" Word64)]
    -> [(PoolId, Quantity "lovelace" Word64)]
    -> Property
prop_putStakePutStake db epoch a b =
    monadicIO (setup >> prop)
  where
    setup = run (cleanDB db)
    prop = do
        run $ putStakeDistribution db epoch a
        run $ putStakeDistribution db epoch b
        res <- run $ readStakeDistribution db epoch
        monitor $ counterexample $ unlines
            [ "Read from DB: " <> show res ]
        monitor $ classify (null a) "a is empty"
        monitor $ classify (null b) "b is empty"
        monitor $ classify (null a && null b) "a & b are empty"
        assert (L.sort res == L.sort b)

descSlotsPerPool :: Map PoolId [BlockHeader] -> Expectation
descSlotsPerPool pools = do
    let checkIfDesc slots =
            L.sortOn Down slots == slots
    let pools' = Map.filter checkIfDesc pools
    pools' `shouldBe` pools

noEmptyPools :: Map PoolId [BlockHeader] -> Expectation
noEmptyPools pools = do
    let pools' = Map.filter (not . null) pools
    pools' `shouldBe` pools

uniqueEpochs :: [(PoolId, BlockHeader)] -> [EpochNo]
uniqueEpochs = nubOrd . map (epochNumber . slotId . snd)

-- | Concatenate stake pool production for all epochs in the test fixture.
allPoolProduction :: DBLayer IO -> StakePoolsFixture -> IO [(SlotId, PoolId)]
allPoolProduction db (StakePoolsFixture pairs _) =
    rearrange <$> mapM (readPoolProduction db) (uniqueEpochs pairs)
  where
    rearrange ms = concat
        [ [ (slotId h, p) | h <- hs ] | (p, hs) <- concatMap Map.assocs ms ]
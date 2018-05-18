{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module InputSelection.Policy (
    -- * Infrastructure
    LiftQuickCheck(..)
  , RunPolicy(..)
  , InputSelectionPolicy
  , PrivacyMode(..)
    -- * Specific policies
  , exactSingleMatchOnly
  , random
  ) where

import           Universum

import           Control.Lens ((.=), (%=))
import           Control.Lens.TH (makeLenses)
import           Control.Monad.Except (MonadError(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Test.QuickCheck

import           UTxO.DSL

{-------------------------------------------------------------------------------
  Auxiliary: lift QuickCheck computations
-------------------------------------------------------------------------------}

-- | Monads in which we can run QuickCheck generators
class Monad m => LiftQuickCheck m where
   -- | Run a QuickCheck computation
  liftQuickCheck :: Gen x -> m x

-- | TODO: We probably don't want this instance (or abstract in a different
-- way over "can generate random numbers")
instance LiftQuickCheck IO where
  liftQuickCheck = generate

instance LiftQuickCheck m => LiftQuickCheck (StateT s m) where
  liftQuickCheck = lift . liftQuickCheck

{-------------------------------------------------------------------------------
  Policy
-------------------------------------------------------------------------------}

-- | Monads in which we can run input selection policies
class Monad m => RunPolicy m a | m -> a where
  -- | Generate change address
  genChangeAddr :: m a

  -- | Generate fresh hash
  genFreshHash :: m Int

type InputSelectionPolicy h a m =
      (RunPolicy m a, Hash h a)
   => Utxo h a -> [Output a] -> m (Either InputSelectionFailure (Transaction h a))

data InputSelectionFailure = InputSelectionFailure

{-------------------------------------------------------------------------------
  Input selection combinator
-------------------------------------------------------------------------------}

data InputPolicyState h a = InputPolicyState {
      -- | Available entries in the UTxO
      _ipsUtxo :: Utxo h a

      -- | Selected inputs
    , _ipsSelectedInputs :: Set (Input h a)

      -- | Generated outputs
    , _ipsGeneratedOutputs :: [Output a]
    }

initInputPolicyState :: Utxo h a -> InputPolicyState h a
initInputPolicyState utxo = InputPolicyState {
      _ipsUtxo             = utxo
    , _ipsSelectedInputs   = Set.empty
    , _ipsGeneratedOutputs = []
    }

makeLenses ''InputPolicyState

newtype InputPolicyT h a m x = InputPolicyT {
      unInputPolicyT :: StateT (InputPolicyState h a) (ExceptT InputSelectionFailure m) x
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState (InputPolicyState h a)
           , MonadError InputSelectionFailure
           )

instance MonadTrans (InputPolicyT h a) where
  lift = InputPolicyT . lift . lift

instance LiftQuickCheck m => LiftQuickCheck (InputPolicyT h a m) where
  liftQuickCheck = lift . liftQuickCheck

instance RunPolicy m a => RunPolicy (InputPolicyT h a m) a where
  genChangeAddr = lift genChangeAddr
  genFreshHash  = lift genFreshHash

runInputPolicyT :: RunPolicy m a
                => Utxo h a
                -> InputPolicyT h a m ()
                -> m (Either InputSelectionFailure (Transaction h a))
runInputPolicyT utxo policy = do
     mx <- runExceptT (execStateT (unInputPolicyT policy) initSt)
     case mx of
       Left err ->
         return $ Left err
       Right finalSt -> do
         h <- genFreshHash
         return $ Right Transaction {
             trFresh = 0
           , trIns   = finalSt ^. ipsSelectedInputs
           , trOuts  = finalSt ^. ipsGeneratedOutputs
           , trFee   = 0 -- TODO: deal with fees
           , trHash  = h
           , trExtra = []
           }
  where
    initSt = initInputPolicyState utxo

{-------------------------------------------------------------------------------
  Exact matches only
-------------------------------------------------------------------------------}

-- | Look for exact single matches only
--
-- Each goal output must be matched by exactly one available output.
exactSingleMatchOnly :: forall h a m. InputSelectionPolicy h a m
exactSingleMatchOnly utxo = \goals -> runInputPolicyT utxo $
    mapM_ go goals
  where
    go :: Output a -> InputPolicyT h a m ()
    go goal@(Output _a val) = do
      i <- useExactMatch val
      ipsSelectedInputs   %= Set.insert i
      ipsGeneratedOutputs %= (goal :)

-- | Look for an input in the UTxO that matches the specified value exactly
useExactMatch :: (Monad m, Hash h a)
              => Value -> InputPolicyT h a m (Input h a)
useExactMatch goalVal = do
    utxo <- utxoToList <$> use ipsUtxo
    case filter (\(_inp, Output _a val) -> val == goalVal) utxo of
      (i, _o) : _ -> ipsUtxo %= utxoDelete i >> return i
      _otherwise -> throwError InputSelectionFailure

{-------------------------------------------------------------------------------
  Random
-------------------------------------------------------------------------------}

data PrivacyMode = PrivacyModeOn | PrivacyModeOff

-- | Random input selection
--
-- Random input selection has the advantage that is it self correcting, in the
-- following sense: suppose that 90% of our UTxO consists of small outputs;
-- then random selection has a 90% change of choosing those small outputs.
--
-- For each output we add a change output that is between 0.5 and 2 times the
-- size of the output, making it hard to identify. This has the additional
-- benefit of introducing another self-correction: if there are frequent
-- requests for payments around certain size, the UTxO will contain lots of
-- available change outputs of around that size.
random :: forall h a m. LiftQuickCheck m
       => PrivacyMode -> InputSelectionPolicy h a m
random privacyMode utxo = \goals -> runInputPolicyT utxo $
    mapM_ go goals
  where
    go :: Output a -> InputPolicyT h a m ()
    go goal@(Output _a val) = do
        -- First attempt to find a change output in the ideal range.
        -- Failing that, try to at least cover the value.
        --
        -- TODO: We should take deposit/payment ratio into account and
        -- change number of change outputs accordingly
        selected <- case privacyMode of
          PrivacyModeOff -> randomInRange fallback
          PrivacyModeOn  -> randomInRange ideal `catchError` \_err ->
                            randomInRange fallback
        ipsSelectedInputs   %= Set.union (utxoDomain selected)
        ipsGeneratedOutputs %= (goal :)
        let selectedSum = utxoBalance selected
            change      = selectedSum - val
        unless (change == 0) $ do
          changeAddr <- genChangeAddr
          ipsGeneratedOutputs %= (Output changeAddr change :)
      where
        changeMin = val `div` 2
        changeMax = val *     2
        ideal     = (val + changeMin, val + changeMax)
        fallback  = (val, maxBound)

-- | Random input selection: core algorithm
--
-- Select random inputs until we reach a value in the given bounds.
-- Returns the selected outputs.
randomInRange :: forall h a m. (Hash h a, LiftQuickCheck m)
              => (Value, Value) -> InputPolicyT h a m (Utxo h a)
randomInRange (lo, hi) =
    go 0 utxoEmpty utxoEmpty
  where
    -- Returns the UTxO that we used to cover the range if successful
    go :: Value    -- ^ Accumulated value
       -> Utxo h a -- ^ Discarded UTxO (not used, but not useable either)
       -> Utxo h a -- ^ Used UTxO
       -> InputPolicyT h a m (Utxo h a)
    go acc discarded used =
      if lo <= acc && acc <= hi
        then do
          ipsUtxo %= utxoUnion discarded -- make discarded available again
          return used
        else do
          io@(_, out) <- useRandomOutput
          let acc' = acc + outVal out
          if acc' <= hi -- should we pick this value?
            then go acc' discarded (utxoInsert io used)
            else go acc  (utxoInsert io discarded) used

useRandomOutput :: LiftQuickCheck m
                => InputPolicyT h a m (Input h a, Output a)
useRandomOutput = do
    utxo <- utxoToMap <$> use ipsUtxo
    mIO  <- liftQuickCheck $ randomElement utxo
    case mIO of
      Nothing          -> throwError InputSelectionFailure
      Just (io, utxo') -> ipsUtxo .= utxoFromMap utxo' >> return io

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Pick a random element from a map
--
-- Returns 'Nothing' if the map is empty
randomElement :: forall k a. Map k a -> Gen (Maybe ((k, a), Map k a))
randomElement m
  | Map.null m = return Nothing
  | otherwise  = (Just . withIx) <$> choose (0, Map.size m - 1)
  where
    withIx :: Int -> ((k, a), Map k a)
    withIx ix = (Map.elemAt ix m, Map.deleteAt ix m)

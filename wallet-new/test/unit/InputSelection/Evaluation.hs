{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module InputSelection.Evaluation (
    evaluateInputPolicies
    -- * Interpreter
  , Stats(..)
  , IntState -- Opaque
  , initIntState
  , intPolicy
  ) where

import           Universum

import           Control.Lens ((%=), (.=), (<<+=))
import           Control.Lens.TH (makeLenses)
import           Data.Conduit
import qualified Data.Conduit.Lift as Conduit
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Formatting (build, sformat, (%))
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>))
import           System.IO (SeekMode (..), hSeek)
import           Text.Printf (printf)

import           InputSelection.Generator (Event (..))
import qualified InputSelection.Generator as Gen
import           InputSelection.Policy (RunPolicy (..), InputSelectionPolicy,
                                        PrivacyMode (..))
import qualified InputSelection.Policy as Policy
import           Util.Histogram (Histogram, BinSize(..))
import qualified Util.Histogram as Histogram
import           UTxO.DSL

{-------------------------------------------------------------------------------
  Observarions we make at each step
-------------------------------------------------------------------------------}

-- | Observe the current state
data Observation h a = Observation {
      -- | Current UTxO
      obsUtxo :: Utxo h a

      -- | Histogram of the current UTxO
    , obsHistogram :: Histogram
    }

mkObservation :: BinSize -> Utxo h a -> Observation h a
mkObservation binSize utxo = Observation {
      obsUtxo      = utxo
    , obsHistogram = utxoHistogram binSize utxo
    }

utxoHistogram :: BinSize -> Utxo h a -> Histogram
utxoHistogram binSize =
    Histogram.discretize binSize . map fromIntegral . outputs
  where
    outputs :: Utxo h a -> [Value]
    outputs = map (outVal . snd) . utxoToList

{-------------------------------------------------------------------------------
  Running statistics
-------------------------------------------------------------------------------}

-- | Accumulated statistics
data Stats = Stats {
      -- | Frame
      --
      -- This is just a simple counter
      statsFrame :: !Int

      -- | Maximum values across all bins
    , statsMaxHistogram :: !Histogram
    }

initStats :: BinSize -> Stats
initStats binSize = Stats {
      statsFrame        = 0
    , statsMaxHistogram = Histogram.empty binSize
    }

-- | Update statistics with most recent observation
updateStats :: Observation h a -> Stats -> Stats
updateStats Observation{..} Stats{..} = Stats {
      statsFrame        = statsFrame + 1
    , statsMaxHistogram = obsHistogram `Histogram.max` statsMaxHistogram
    }

{-------------------------------------------------------------------------------
  Interpreter state
-------------------------------------------------------------------------------}

data IntState h a = IntState {
      _stUtxo       :: Utxo h a
    , _stPending    :: Utxo h a
    , _stStats      :: Stats
    , _stFreshHash  :: Int

      -- | Change address
      --
      -- NOTE: At the moment we never modify this; we're not evaluating
      -- privacy, so change to a single address is fine.
    , _stChangeAddr :: a

      -- | Binsize used for histograms
      --
      -- We cannot actually currently change this as we run the interpreter
      -- because `Histogram.max` only applies to histograms wit equal binsizes.
    , _stBinSize :: BinSize
    }

makeLenses ''IntState

initIntState :: BinSize -> Utxo h a -> a -> IntState h a
initIntState binSize utxo changeAddr = IntState {
      _stUtxo       = utxo
    , _stPending    = utxoEmpty
    , _stStats      = initStats binSize
    , _stFreshHash  = 1
    , _stChangeAddr = changeAddr
    , _stBinSize    = binSize
    }

instance Monad m => RunPolicy (StateT (IntState h a) m) a where
  genChangeAddr = use stChangeAddr
  genFreshHash  = stFreshHash <<+= 1

{-------------------------------------------------------------------------------
  Interpreter proper
-------------------------------------------------------------------------------}

-- | Construct an observation and update the statistics
observe :: Monad m => StateT (IntState h a) m (Observation h a, Stats)
observe = state aux
  where
    aux :: IntState h a -> ((Observation h a, Stats), IntState h a)
    aux st = ((obs, stats), st & stStats .~ stats)
      where
        obs   = mkObservation (st ^. stBinSize) (st ^. stUtxo)
        stats = updateStats obs (st ^. stStats)

-- | Interpreter for events, evaluating a policy
--
-- Turns a stream of events into a stream of observations and accumulated
-- statistics.
--
-- Returns the final state
intPolicy :: forall h a m. (Hash h a, Monad m)
          => InputSelectionPolicy h a (StateT (IntState h a) m)
          -> IntState h a -- Initial state
          -> ConduitT (Event h a) (Observation h a, Stats) m (IntState h a)
intPolicy policy initState =
    Conduit.execStateC initState $
      awaitForever $ \event -> do
        lift $ go event
        yield =<< lift observe
  where
    go :: Event h a -> StateT (IntState h a) m ()
    go (Deposit new) =
        stUtxo %= utxoUnion new
    go NextSlot = do
        -- TODO: May want to commit only part of the pending transactions
        pending <- use stPending
        stUtxo    %= utxoUnion pending
        stPending .= utxoEmpty
    go (Pay outs) = do
        utxo <- use stUtxo
        mtx  <- policy utxo outs
        case mtx of
          Right tx -> do
            stUtxo    %= utxoRemoveInputs (trIns tx)
            stPending %= utxoUnion (trUtxo tx)
          Left _err ->
            -- TODO: record some stats
            return ()

{-------------------------------------------------------------------------------
  Render observations
-------------------------------------------------------------------------------}

-- | Sink that writes observations to disk
render :: forall h a m. MonadIO m
       => Handle   -- ^ Handle to write gnuplot script instructions to
       -> FilePath -- ^ Prefix for the files to create
       -> ConduitT (Observation h a, Stats) Void m ()
render hGnuplot prefix =
    awaitForever (liftIO . go)
  where
    go :: (Observation h a, Stats) -> IO ()
    go (Observation{..}, Stats{..}) = do
        Histogram.writeFile filepath obsHistogram
        Text.hPutStrLn hGnuplot plotInstrs
      where
        filename = printf "%08d" statsFrame
        filepath = prefix </> filename

        plotInstrs :: Text
        plotInstrs = sformat
          ( "set output '" % build % ".png'\n"
          % "plot '" % build % "' using 1:2 with boxes\n"
          )
          filename
          filename

{-------------------------------------------------------------------------------
  Run evaluation
-------------------------------------------------------------------------------}

evaluatePolicy :: Hash h a
               => FilePath
               -> InputSelectionPolicy h a (StateT (IntState h a) IO)
               -> IntState h a
               -> ConduitT () (Event h a) IO ()
               -> IO ()
evaluatePolicy prefix policy initState generator = do
    createDirectoryIfMissing False prefix
    withFile gnuplotScript WriteMode $ \hGnuplot -> do
      -- Leave space for plot instructions
      -- (which we know only after we have seen all histograms)
      Text.hPutStrLn hGnuplot $ Text.replicate 200 " "

      finalState <- runConduit $
        generator                  `fuse`
        intPolicy policy initState `fuseUpstream`
        render hGnuplot prefix

      let Stats{..}        = finalState ^. stStats
          (xRange, yRange) = Histogram.range statsMaxHistogram

      hSeek hGnuplot AbsoluteSeek 0
      Text.hPutStrLn hGnuplot $ sformat
          ( "set grid\n"
          % "set xrange " % build % "\n"
          % "set yrange " % build % "\n"
          % "set boxwidth " % build % "\n"
          % "set term png\n"
          )
          xRange
          yRange
          (finalState ^. stBinSize)

      putStrLn $ sformat ("Written '" % build % "'") gnuplotScript
  where
    gnuplotScript = prefix </> "mkframes.gnuplot"


evaluateInputPolicies :: FilePath -> IO ()
evaluateInputPolicies prefix = do
    evaluatePolicy
      (prefix </> "exact")
      Policy.exactSingleMatchOnly
      (initIntState (BinSize 10) utxoEmpty ())
      (Gen.test Gen.defTestParams)

    evaluatePolicy
      (prefix </> "trivialOff")
      (Policy.random PrivacyModeOff)
      (initIntState (BinSize 10) utxoEmpty ())
      (Gen.trivial 1000 100 1000)
    evaluatePolicy
      (prefix </> "trivialOn")
      (Policy.random PrivacyModeOn)
      (initIntState (BinSize 10) utxoEmpty ())
      (Gen.trivial 1000 100 1000)



{-

input selection
coin selection

bitcoinj coin selection? ("multiple classes" -- multiple policies)

https://github.com/bitcoin/bitcoin/issues/7664

See ApproximateBestSubset in wallet.cpp.

sweep?



-}


{-
http://murch.one/wp-content/uploads/2016/11/erhardt2016coinselection.pdf
"An Evaluation of Coin Selection Strategies", Master’s Thesis, Mark Erhardt

2.3.4
A transaction output is labeled as dust when its value is similar to the cost of
spending it.Precisely, Bitcoin Core sets the dust limit to a value where spending an
2.3. Transactions 7
output would exceed 1/3 of its value. T

https://youtu.be/vnHQwYxB08Y?t=39m


https://www.youtube.com/watch?v=IKSSWUBqMCM

companies; hundreds of entries in UTxO
individuals: tens of entries

batch payments!
  (Harding, BitcoinTechTalk -- safe up to 80% of transaction fees)

coin selection --
  relatively minor importance
    batching, better representation (SegWit), .. much larger impact
    coin selection only a few percent savings

* FIFO is actually a reasonable strategy (!)
* So is random
    self correcting -- if you have a large amount of small inputs,
    they'll be more likely to be picked!
    (i.e, if 90% of the wallet is small inputs, 90% change of picking them!)

Branch&Bound seems to do exhaustive search (backtracking algorithm) to find
exact matches, coupled with random selection.

A Traceability Analysis of Monero’s Blockchain
https://eprint.iacr.org/2017/338.pdf
-}

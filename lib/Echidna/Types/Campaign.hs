module Echidna.Types.Campaign where

import Control.Concurrent (ThreadId)
import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16, Word8)
import Echidna.ABI (GenDict, emptyDict, encodeSig)
import Echidna.Types
import Echidna.Types.Coverage (CoverageFileType, CoverageMap)
import Echidna.Types.Test (EchidnaTest (..), TestType (..))
import Echidna.Types.Tx (Tx)
import GHC.Conc (numCapabilities)

-- | Configuration for running an Echidna 'Campaign'.
data CampaignConf = CampaignConf
  { -- | Maximum number of function calls to execute while fuzzing
    testLimit :: Int,
    -- | Whether to stop the campaign immediately if any property fails
    stopOnFail :: Bool,
    -- | Whether to collect gas usage statistics
    estimateGas :: Bool,
    -- | Number of calls between state resets (e.g. \"every 10 calls,
    -- reset the state to avoid unrecoverable states/save memory\"
    seqLen :: Int,
    -- | Filepath of state machine json, used to reduce invalid call chain, nullable
    stateMachineJson :: Maybe String,
    -- | Maximum number of candidate sequences to evaluate while shrinking
    shrinkLimit :: Int,
    -- | If applicable, initially known coverage. If this is 'Nothing',
    -- Echidna won't collect coverage information (and will go faster)
    knownCoverage :: Maybe CoverageMap,
    -- | Seed used for the generation of random transactions
    seed :: Maybe Int,
    -- | Frequency for the use of dictionary values in the random transactions
    dictFreq :: Float,
    -- | Directory to load and save lists of transactions
    corpusDir :: Maybe FilePath,
    -- | Directory to load and save lists of transactions
    mutConsts :: MutationConsts Integer,
    -- | List of file formats to save coverage reports
    coverageFormats :: [CoverageFileType],
    -- | Number of fuzzing workers
    workers :: Maybe Word8,
    -- | Server-Sent Events HTTP port number, if missing server is not ran
    serverPort :: Maybe Word16,
    -- | Whether to add an additional symbolic execution worker
    symExec :: Bool,
    -- | Whether symbolic execution will be concolic (vs full symbolic execution)
    -- Only relevant if symExec is True
    symExecConcolic :: Bool,
    symExecTargets :: Maybe [Text],
    -- | Timeout for symbolic execution SMT solver queries.
    -- Only relevant if symExec is True
    symExecTimeout :: Int,
    -- | Number of SMT solvers used in symbolic execution.
    -- Only relevant if symExec is True
    symExecNSolvers :: Int,
    -- | Number of times we may revisit a particular branching point.
    -- Only relevant if symExec is True and symExecConcolic is False
    symExecMaxIters :: Integer,
    -- | Number of times we may revisit a particular branching point
    -- before we consult the SMT solver to check reachability.
    -- Only relevant if symExec is True and symExecConcolic is False
    symExecAskSMTIters :: Integer
  }

data WorkerType = FuzzWorker | SymbolicWorker deriving (Eq)

type WorkerId = Int

data CampaignEvent
  = WorkerEvent WorkerId WorkerType WorkerEvent
  | Failure String
  | ReproducerSaved String -- filename

data WorkerEvent
  = TestFalsified !EchidnaTest
  | TestOptimized !EchidnaTest
  | NewCoverage {points :: !Int, numCodehashes :: !Int, corpusSize :: !Int, transactions :: [Tx]}
  | SymNoNewCoverage
  | TxSequenceReplayed FilePath !Int !Int
  | TxSequenceReplayFailed FilePath Tx
  | -- | This is a terminal event. Worker exits and won't push any events after
    -- this one
    WorkerStopped WorkerStopReason
  deriving (Show)

instance ToJSON WorkerEvent where
  toJSON = \case
    TestFalsified test -> toJSON test
    TestOptimized test -> toJSON test
    NewCoverage {points, numCodehashes, corpusSize} ->
      object ["coverage" .= points, "contracts" .= numCodehashes, "corpus_size" .= corpusSize]
    SymNoNewCoverage -> object []
    TxSequenceReplayed file current total ->
      object ["file" .= file, "current" .= current, "total" .= total]
    TxSequenceReplayFailed file tx ->
      object ["file" .= file, "tx" .= tx]
    WorkerStopped reason -> object ["reason" .= show reason]

data WorkerStopReason
  = TestLimitReached
  | SymbolicDone
  | TimeLimitReached
  | FastFailed
  | Killed !String
  | Crashed !String
  deriving (Show)

ppCampaignEvent :: CampaignEvent -> String
ppCampaignEvent = \case
  WorkerEvent _ _ e -> ppWorkerEvent e
  Failure err -> err
  ReproducerSaved f -> "Saved reproducer to " <> f

ppWorkerEvent :: WorkerEvent -> String
ppWorkerEvent = \case
  TestFalsified test ->
    "Test " <> T.unpack (showTest test) <> " falsified!"
  TestOptimized test ->
    let name = case test.testType of OptimizationTest n _ -> n; _ -> error "fixme"
     in "New maximum value of " <> T.unpack name <> ": " <> show test.value
  NewCoverage {points, numCodehashes, corpusSize} ->
    "New coverage: "
      <> show points
      <> " instr, "
      <> show numCodehashes
      <> " contracts, "
      <> show corpusSize
      <> " seqs in corpus"
  SymNoNewCoverage ->
    "Symbolic execution finished with no new coverage."
  TxSequenceReplayed file current total ->
    "Sequence replayed from corpus file " <> file <> " (" <> show current <> "/" <> show total <> ")"
  TxSequenceReplayFailed file tx ->
    "WARNING: Sequence replay from corpus file "
      <> file
      <> " failed. "
      <> "The destination contract is not deployed for this transaction: "
      <> show tx
      <> ". "
      <> "Remove the file or the transaction to fix the issue."
  WorkerStopped TestLimitReached ->
    "Test limit reached. Stopping."
  WorkerStopped SymbolicDone ->
    "Symbolic worker ran out of transactions to work on. Stopping."
  WorkerStopped TimeLimitReached ->
    "Time limit reached. Stopping."
  WorkerStopped FastFailed ->
    "A test was falsified. Stopping."
  WorkerStopped (Killed e) ->
    "Killed (" <> e <> "). Stopping."
  WorkerStopped (Crashed e) ->
    "Crashed:\n\n"
      <> e
      <> "\n\nPlease report it to https://github.com/crytic/echidna/issues"
  where
    showTest test = case test.testType of
      PropertyTest n _ -> n
      AssertionTest _ n _ -> encodeSig n
      CallTest n _ -> n
      _ -> error "impossible"

-- | The state of a fuzzing campaign.
data WorkerState = WorkerState
  { -- | Worker ID starting from 0
    workerId :: !Int,
    -- | Worst case gas (NOTE: we don't always record this)
    gasInfo :: !(Map Text (Gas, [Tx])),
    -- | Generation dictionary
    genDict :: !GenDict,
    -- | Flag to indicate new coverage found
    newCoverage :: !Bool,
    -- | Number of times the callseq is called
    ncallseqs :: !Int,
    -- | Number of calls executed while fuzzing
    ncalls :: !Int,
    -- | Flag to indicate that the callseq has reverted
    callseqReverted :: !Bool,
    -- | Number of calls executed before the first revert
    revertAt :: !Int,
    -- | Total gas consumed while fuzzing
    totalGas :: !Int,
    -- | Extra threads currently being run,
    --   aside from the main worker thread
    runningThreads :: [ThreadId]
  }

initialWorkerState :: WorkerState
initialWorkerState =
  WorkerState
    { workerId = 0,
      gasInfo = mempty,
      genDict = emptyDict,
      newCoverage = False,
      ncallseqs = 0,
      ncalls = 0,
      callseqReverted = False,
      revertAt = 0,
      totalGas = 0,
      runningThreads = []
    }

defaultTestLimit :: Int
defaultTestLimit = 50000

defaultSequenceLength :: Int
defaultSequenceLength = 100

defaultShrinkLimit :: Int
defaultShrinkLimit = 5000

defaultSymExecTimeout :: Int
defaultSymExecTimeout = 30

defaultSymExecNWorkers :: Int
defaultSymExecNWorkers = 1

defaultSymExecMaxIters :: Integer
defaultSymExecMaxIters = 10

-- | Same default as in hevm, "everything else is unsound"
-- (https://github.com/ethereum/hevm/pull/252)
defaultSymExecAskSMTIters :: Integer
defaultSymExecAskSMTIters = 1

defaultJSONFilePath :: String
defaultJSONFilePath = ""

defaultStateMachineJson :: Maybe String
defaultStateMachineJson = Nothing

-- defaultStateMachine :: Maybe StateMachine
-- defaultStateMachine = Nothing

-- | Get number of fuzzing workers (doesn't include sym exec worker)
-- Defaults to `N` if set to Nothing, where `N` is Haskell's -N value,
-- usually the number of cores, clamped between 1 and 4.
getNFuzzWorkers :: CampaignConf -> Int
getNFuzzWorkers conf = maybe defaultN fromIntegral conf.workers
  where
    n = numCapabilities
    maxN = max 1 n
    defaultN = min 4 maxN -- capped at 4 by default

-- | Number of workers, including SymExec worker if there is one
getNWorkers :: CampaignConf -> Int
getNWorkers conf = getNFuzzWorkers conf + (if conf.symExec then 1 else 0)

workerIDToType :: CampaignConf -> WorkerId -> WorkerType
workerIDToType conf wid = if conf.symExec && wid == (getNWorkers conf - 1) then SymbolicWorker else FuzzWorker

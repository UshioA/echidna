module Echidna.Types.StateMachine where

import Control.Monad (mzero)
import Data.Aeson.Types
import Data.Map.Strict qualified as HM
import Data.Text qualified as T

-- 定义数据结构
data StateMachine = StateMachine
  { states :: [T.Text],
    transitionsAccept :: HM.Map T.Text [T.Text],
    transitionsReject :: HM.Map T.Text [T.Text]
  }
  deriving (Show)

instance FromJSON StateMachine where
  parseJSON :: Value -> Parser StateMachine
  parseJSON (Object v) = do
    stateMachine <- v .: "state_machine"
    states <- stateMachine .: "states"
    transitionsAccept <- stateMachine .:? "transitions_accept" .!= HM.empty
    transitionsReject <- stateMachine .:? "transitions_reject" .!= HM.empty
    return $ StateMachine states transitionsAccept transitionsReject
  parseJSON _ = mzero
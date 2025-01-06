module Echidna.StateMachine where

import Control.Monad (mzero)
import Data.Aeson (eitherDecode)
import Data.Aeson.Types
import Data.ByteString.Lazy qualified as B
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

-- 读取 JSON 文件并解析
readJsonFile :: FilePath -> IO (Either String StateMachine)
readJsonFile filePath = do
  jsonData <- B.readFile filePath
  let parsed = eitherDecode jsonData :: Either String StateMachine
  return parsed

-- main :: IO ()
-- main = do
--   let filePath = "/home/hengdiye/tools/echidna/output_20241201_172300_002d5f79eff9f8d6d47a61fdaf5bf0c6.sol.json"
--   result <- readJsonFile filePath
--   case result of
--     Left err -> putStrLn $ "Error: " ++ err
--     Right stateMachine -> print stateMachine
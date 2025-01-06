module Echidna.StateMachine where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as B
import Echidna.Types.StateMachine

-- 读取 JSON 文件并解析
readJsonFile :: FilePath -> IO (Either String StateMachine)
readJsonFile filePath = do
  jsonData <- B.readFile filePath
  let parsed = eitherDecode jsonData :: Either String StateMachine
  return parsed
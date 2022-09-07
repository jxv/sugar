module Sugar.Cache where

import qualified Data.Map as Map
import qualified Control.Concurrent.STM.Map as STM

import Sugar
import Control.Concurrent.STM

data Cache = Cache
  { values :: TVar (Map.Map Sugar Sugar)
  , lastEdited :: TVar UTCTime
  , storageRoot :: Maybe FilePath
  } deriving (Show, Eq)

data Op
  -- Basic
  = Set   -- set:{key:value, key2:value2, ...}
  | Get   -- get:key
          -- get:[key, key2, ...]
  | Rm    -- rm:key
          -- rm:[key, key2, ...]
  | Keys  -- keys:()
  | All   -- all:()
  -- Set
  | SAdd  -- sadd:{key:value, key2:value2, ...}
  | SGet  -- sget:key
          -- sget:[key, key2, ...]
  | SRm   -- srm:{key:value, key2:value2, ...}
          -- srm:[key, key, ..]   
  -- Hash
  deriving (Show, Eq)

data Query = Query
  { op :: Op
  } deriving (Show, Eq)

  -- Prim, Set, Hash, MultiHash, List

main :: IO ()
main = return ()

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
module State_v2 where

import qualified State as V1
import qualified Data.Sequence as Seq
import Data.Sequence(Seq, viewl, ViewL(..), (|>))
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Typeable
import Data.Acid
import Control.Monad.Reader(ask)
import Control.Monad.State(get, put)
import Data.SafeCopy

data CheckResult = TypeCheckSuccess | TypeCheckFailure String deriving (Show, Typeable)

data Database = Database {
  submissions :: Seq String,
  pending :: Seq Int,
  checked :: Map Int CheckResult
  } deriving (Show, Typeable)

addSubmission :: String -> Update Database Int
addSubmission text = do
  Database { submissions, pending, checked } <- get
  let i = Seq.length submissions
  let submission = V1.replace "module SUBMISSION" ("module S" ++ show i) text
  put $ Database { submissions = submissions |> submission, pending = pending |> i, checked }
  return i

reportPendingResult :: Int -> CheckResult -> Update Database ()
reportPendingResult id isSuccess = do
  Database { submissions, pending, checked } <- get
  case viewl pending of
    (a :< t) | a == id -> put $ Database { submissions, pending = t, checked = Map.insert id isSuccess checked }
    _ -> return ()


readDb :: Query Database Database
readDb = ask

instance Migrate Database where
  type MigrateFrom Database = V1.Database
  migrate (V1.Database {V1.submissions, V1.pending, V1.checked}) = Database{
    submissions
    , pending
    , checked = Map.map (\b -> case b of
                  False -> TypeCheckFailure "Old agdapaste did not save errors"
                  True -> TypeCheckSuccess) checked
    }

$(deriveSafeCopy 2 'base ''CheckResult) 
$(deriveSafeCopy 2 'extension ''Database)

$(makeAcidic ''Database ['addSubmission, 'readDb, 'reportPendingResult])
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
module State where

import Data.Sequence (Seq, (|>), viewl, ViewL(..))
import qualified Data.Sequence as Seq
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                   ( ask )
import Data.List
import Data.Typeable

import Data.Acid
import Data.SafeCopy

data Database = Database {
  submissions :: Seq String,
  pending :: Seq Int,
  checked :: Map Int Bool
  } deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''Database)

replace :: String -> String -> String -> String
replace a b s | a `isPrefixOf` s = b ++ replace a b (drop (length a) s)
replace a b (h:s) = h : replace a b s
replace a b [] = []
  
addSubmission :: String -> Update Database Int
addSubmission text = do
  Database { submissions, pending, checked } <- get
  let i = Seq.length submissions
  let submission = replace "module SUBMISSION" ("module S" ++ show i) text
  put $ Database { submissions = submissions |> submission, pending = pending |> i, checked }
  return i

reportPendingResult :: Int -> Bool -> Update Database ()
reportPendingResult id isSuccess = do
  Database { submissions, pending, checked } <- get
  case viewl pending of
    (a :< t) | a == id -> put $ Database { submissions, pending = t, checked = Map.insert id isSuccess checked }
    _ -> return ()

readDb :: Query Database Database
readDb = ask

$(makeAcidic ''Database ['addSubmission, 'readDb, 'reportPendingResult])

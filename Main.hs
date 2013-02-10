{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Time.Clock
import Control.Applicative
import State_v2
import qualified State as State_V1
import Data.Acid
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Sequence(index, ViewL(..))
import Control.Concurrent(threadDelay, forkIO)
import Control.Monad
import System.Process
import System.Exit
import Web
import Paths

checkSubmission :: AcidState Database -> Int -> String -> IO ()
checkSubmission acid id submission = do
  writeFile (submissionPath id) submission
  putStrLn $ "Running Agda for submission " ++ show id ++ "..."
  (result, agdaOutput, _) <- readProcessWithExitCode "agda" (agdaCmdLine id) ""
  putStrLn $ "The result was: " ++ show result
  let isSuccess = case result of
        ExitSuccess -> TypeCheckSuccess
        ExitFailure _ -> TypeCheckFailure agdaOutput
  time <- getCurrentTime
  update acid (ReportPendingResult id (time, isSuccess))
  

checkerOnce :: AcidState Database -> IO ()
checkerOnce acid = do
  Database { pending, submissions } <- query acid ReadDb
  case Seq.viewl pending of
    EmptyL -> return ()
    a :< _ -> checkSubmission acid a (submissionBody $ index submissions a)

-- | A process running forever, typechecking Agda programs and collecting results
checker :: AcidState Database -> IO a
checker acid = forever $ do
  threadDelay 100
  checkerOnce acid

getLineUntilZOGO = do
  l <- getLine
  if (l=="ZOGO")
    then return []
    else (l:) <$> getLineUntilZOGO

main = do
  acid <- openLocalState (Database { submissions = Seq.empty
                                  , pending = Seq.empty
                                  })
  forkIO (checker acid)
  runWeb acid

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
module State_v2 where

import qualified State as V1
import qualified Data.Sequence as Seq
import Data.Sequence(Seq, viewl, ViewL(..), (|>), (<|), (><))
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Typeable
import Data.Traversable
import Data.Acid
import Control.Monad.Reader(ask)
import Control.Monad.State(get, put)
import Control.Arrow
import Data.SafeCopy
import Data.Char
import Data.List
import Data.Time.Clock
import Text.ParserCombinators.Poly

data CheckResult = TypeCheckSuccess | TypeCheckFailure String deriving (Show, Typeable)

data Submission = Submission {
  submissionBody :: String,
  submissionDate :: UTCTime,
  submissionChecked :: Maybe (UTCTime, CheckResult)
  } deriving (Show, Typeable)

data Database = Database {
  submissions :: Seq Submission,
  pending :: Seq Int
  } deriving (Show, Typeable)

string = traverse (\c -> satisfy (==c))

preprocess i s = 
  case runParser 
    ((,,) 
     <$> (many $ satisfy isSpace) <* string "module" 
     <*> (many1 $ satisfy isSpace) 
     <*> (many1 $ satisfy isAlphaNum)) s of
      (Left _, _) -> "module S" ++ show i ++ " where\n" ++ s
      (Right (space1, space2, modulename), rest) -> space1 ++ "module" ++ space2 ++ "S" ++ show i ++ rest

fixModule i = skipAll " " $ skipOne "module" $ skipAll " " $ modifyToken (const $ "S" ++ show i) where
skipAll s f x = case stripPrefix s x of
  Nothing -> f x
  Just x' -> s ++ skipAll s f x'
skipOne s f x = case stripPrefix s x of
  Just x' -> s ++ f x'
modifyToken f s = case span isAlphaNum s of
  (tok, rest) -> f tok ++ rest

newSubmission (t, s) = Submission s t Nothing

addSubmission :: (UTCTime, String) -> Update Database Int
addSubmission text = do
  Database { submissions, pending } <- get
  let i = Seq.length submissions
  let submission = second (preprocess i) text
  put $ Database { submissions = submissions |> newSubmission submission, pending = pending |> i }
  return i

modifyAt i f seq = case Seq.splitAt i seq of
  (l, mr) -> case viewl mr of
    m :< r -> l >< (f m <| r)
    EmptyL -> l

modifySubmissionChecked f s @ Submission { submissionChecked = sc }  = s { submissionChecked = f sc }

reportPendingResult :: Int -> (UTCTime, CheckResult) -> Update Database ()
reportPendingResult i result = do
  db @ Database { submissions, pending } <- get
  put $ db 
    { submissions = modifyAt i (modifySubmissionChecked (const $ Just result)) submissions
    , pending = removeFirst i pending }
 where
  removeFirst x seq = case viewl seq of
    (a :< t) | a == x -> t
      | otherwise -> a <| removeFirst x t
    _ -> seq


readDb :: Query Database Database
readDb = ask

$(deriveSafeCopy 2 'base ''CheckResult) 
$(deriveSafeCopy 2 'base ''Database)
$(deriveSafeCopy 2 'base ''Submission)

$(makeAcidic ''Database ['addSubmission, 'readDb, 'reportPendingResult])

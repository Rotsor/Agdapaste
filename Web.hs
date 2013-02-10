{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Web where

import Yesod
import Control.Applicative
import Yesod.Form
import Data.Text hiding (length, mapAccumL)
import Control.Monad.Reader
import Data.Acid
import Data.Time.Clock
import State_v2
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Yesod.Static
import Paths
import Data.Time.Calendar
import Data.Traversable


data AgdaPaste = AgdaPaste {
  agdapAcid :: AcidState Database,
  agdapStatic :: Static
  }

getAcid = agdapAcid <$> getYesod

$(mkYesod "AgdaPaste" [parseRoutes|
/ RootR GET
/submit SubmitR POST
/status GlobalStatusR GET
/status/#Int StatusR GET
/source/#Int SourceR GET
/error/#Int CompileErrorR GET
/html HtmlR Static agdapStatic
|])
instance Yesod AgdaPaste where approot = ApprootStatic ""

instance RenderMessage AgdaPaste FormMessage where
  renderMessage _ _ = defaultFormMessage

agdaForm :: Html -> MForm AgdaPaste AgdaPaste (FormResult Text, Widget)
agdaForm = renderDivs $ 
  ((unTextarea <$> areq textareaField "Source" {fsId=Just "Source"} (Just (Textarea "module SUBMISSION where\n\n"))))

postSubmitR :: Handler RepHtml
postSubmitR = do
  ((result, xml), enctype) <- runFormPost agdaForm
  case result of
    FormSuccess contents -> do
      acid <- getAcid
      id <- liftIO $ join $ Data.Acid.update acid <$> (AddSubmission <$> ((,) <$> getCurrentTime <*> pure (unpack contents)))
      liftIO $ putStrLn $ unpack $ contents
      redirect (StatusR id)
      defaultLayout $ [whamlet|
                 <p>Thank you for your submission!|]
    _ -> showForm enctype xml


seqLookup i seq | i < 0 || i >= Seq.length seq = Nothing
 | otherwise = Just $ Seq.index seq i

noSubmission i = defaultLayout $ [whamlet|<p>No such problem: #{show i}!|]

getSourceR :: Int -> Handler RepHtml
getSourceR i = withSubmission i $ \Submission {submissionBody} -> defaultLayout [whamlet|<pre>#{submissionBody}|]

htmlRFromId id = HtmlR $ StaticRoute [pack $ "S" ++ show id ++ ".html"] []

traverseWithIndex :: (Traversable t, Applicative f) => (Int -> a -> f b) -> t a -> f (t b)
traverseWithIndex f x = traverse (uncurry f) $ snd $ mapAccumL (\i x -> (i+1, (i, x))) 0 x

getGlobalStatusR :: Handler RepHtml
getGlobalStatusR = do
  Database { submissions } <- getDB
  defaultLayout $ do
    let 
      tBody = (const () <$>) $ flip traverseWithIndex submissions $
        (\i Submission { submissionBody, submissionDate, submissionChecked } -> 
            let size = length submissionBody
                status = case submissionChecked of
                  Nothing -> [whamlet|Unchecked...|]
                  Just (date, result) -> [whamlet|<a href=@{link}>#{text} on #{show date}|] where
                     (link, text) = case result of
                       TypeCheckFailure _ -> (CompileErrorR i, "Failed" :: Text)
                       TypeCheckSuccess -> (htmlRFromId i, "Succeeded")
                in
            [whamlet|
            <tr>
             <td>
               <a href=@{SourceR i}>##{show i}
             <td>
               #{show size}
             <td>
               #{show submissionDate}
             <td>
               ^{status}
            |]
            )
    [whamlet|
     <table>
       <tr>
         <th>id
         <th>size
         <th>date
         <th>status
       ^{tBody}|]

getDB :: Handler Database
getDB = do
  acid <- getAcid
  liftIO $ Data.Acid.query acid ReadDb

getStatusR :: Int -> Handler RepHtml
getStatusR i = withSubmission i $ \s -> do
  setHeader "refresh" "2"
  case snd <$> submissionChecked s of
    Nothing -> defaultLayout $
      [whamlet|<p>The submission #{show i} is still being typechecked.|]
    Just TypeCheckSuccess -> redirect (htmlRFromId i)
    Just (TypeCheckFailure message) -> redirect (CompileErrorR i)

getSubmission :: Int -> Handler (Maybe Submission)
getSubmission i = seqLookup i . submissions <$> getDB

withSubmission :: Int -> (Submission -> Handler RepHtml) -> Handler RepHtml
withSubmission i h =
  maybe (noSubmission i) h =<< getSubmission i

getCompileErrorR :: Int -> Handler RepHtml
getCompileErrorR id = withSubmission id $ \s -> do
  case submissionChecked s of
    Just (_, TypeCheckFailure message) ->  defaultLayout [whamlet|
                                                      <p>Type check failure for submission #{show id}:
                                                      <pre>#{message}|]
    _ -> defaultLayout [whamlet|There is no type check failure for submission #{show id}|]

   

showForm enctype xml = do
    defaultLayout $ do
      toWidget [lucius|
        #Source {
            min-width:500px;
            min-height:500px;
        }|]

      [whamlet|
                 <p>
                   <a href=@{GlobalStatusR}>
                     Status
                 <form method=post action=@{SubmitR} enctype="#{enctype}">
                       \^{xml}
                       <input type="submit">
  |]


getRootR :: Handler RepHtml
getRootR = do
    (wform, enctype) <- generateFormPost $ agdaForm
    showForm enctype wform

runWeb acid = do
  stat <- static htmlPath
  warpDebug 3000 (AgdaPaste acid stat)

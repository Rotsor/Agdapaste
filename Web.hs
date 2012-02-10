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
import Data.Text hiding (length)
import Control.Monad.Reader
import Data.Acid
import State_v2
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Yesod.Static
import Paths


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
agdaForm = renderDivs $ unTextarea <$> areq textareaField "Source" {fsId=Just "Source"} (Just (Textarea "module SUBMISSION where\n\n"))

postSubmitR :: Handler RepHtml
postSubmitR = do
  ((result, _), _) <- runFormPost agdaForm
  case result of
    FormSuccess contents -> do
      acid <- getAcid
      id <- liftIO $ Data.Acid.update acid (AddSubmission $ unpack $ contents)
      liftIO $ putStrLn $ unpack $ contents
      redirect (StatusR id)
    _ -> return ()
  defaultLayout [whamlet|
                 <p>Thank you for your submission!|]

seqLookup i seq | i < 0 || i >= Seq.length seq = Nothing
 | otherwise = Just $ Seq.index seq i

getSourceR :: Int -> Handler RepHtml
getSourceR i = do
  Database { submissions } <- getDB
  defaultLayout $ case seqLookup i submissions of
    Nothing -> [whamlet|<p>No such problem!|]
    Just source -> [whamlet|<pre>#{source}|]

htmlRFromId id = HtmlR $ StaticRoute [pack $ "S" ++ show id ++ ".html"] []

getGlobalStatusR :: Handler RepHtml
getGlobalStatusR = do
  Database { submissions, checked } <- getDB
  defaultLayout $ do
    let tBody = forM_ [0 .. Seq.length submissions - 1]
           (\i -> 
            let size = length $ Seq.index submissions i
                status = case Map.lookup i checked of
                  Nothing -> [whamlet|Unchecked...|]
                  Just (TypeCheckFailure _) -> [whamlet|<a href=@{CompileErrorR i}
                                                            Failed|]
                  Just TypeCheckSuccess -> [whamlet|<a href=@{htmlRFromId i}>Succeeded|]
                in
            [whamlet|
            <tr> 
             <td>
               <a href=@{SourceR i}>##{show i}
             <td>
               #{show size}
             <td>
               ^{status}
            |]
            )
    return ()
    [whamlet|
     <table>
       <tr>
         <th>id
         <th>size
         <th>status
       ^{tBody}|]

getDB :: Handler Database
getDB = do
  acid <- getAcid
  liftIO $ Data.Acid.query acid ReadDb

getStatusR :: Int -> Handler RepHtml
getStatusR x = do
  Database { checked } <- getDB
  case Map.lookup x checked of
    Nothing -> defaultLayout $
      [whamlet|<p>The submission #{show x} is still being typechecked.|]
    Just TypeCheckSuccess -> redirect (htmlRFromId x)
    Just (TypeCheckFailure message) -> redirect (CompileErrorR x)

getCompileErrorR :: Int -> Handler RepHtml
getCompileErrorR id = do
  Database { checked } <- getDB
  case Map.lookup id checked of
    Just (TypeCheckFailure message) ->  defaultLayout [whamlet|
                                                      <p>Type check failure for submission #{show id}:
                                                      <pre>#{message}|]
    _ -> defaultLayout [whamlet|There is no type check failure for submission #{show id}|]

   

getRootR :: Handler RepHtml
getRootR = do
    ((_, wform), enctype) <- generateFormPost $ agdaForm
    defaultLayout $ do
      addLucius [lucius|
        #Source {
            min-width:500px;
            min-height:500px;
        }|]

      [whamlet|
                 <p>
                   <a href=@{GlobalStatusR}>
                     Status
                 <form method=post action=@{SubmitR} enctype="#{enctype}">
                       \^{wform}
                       <input type="submit">
|]

runWeb acid = do
  stat <- static htmlPath
  warpDebug 3000 (AgdaPaste acid stat)

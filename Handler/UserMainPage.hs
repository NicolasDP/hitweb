{-# LANGUAGE OverloadedStrings #-}
module Handler.UserMainPage where

import Import
import Data.Text as T

internalConcatText :: Text -> Text -> Text
internalConcatText a b = T.concat [a, "/", b]

getUserMainPageR :: Text -> Handler Html
getUserMainPageR login = do
    extra <- getExtra
    let projDir = extraProjectsDir extra
    defaultLayout $ do
        aDomId <- newIdent
        projectsList <- liftIO $ listProjectIn projDir login
        setTitle $ toHtml $ "hitweb - " ++ (T.unpack login)
        $(widgetFile "project-list")

postUserMainPageR :: Text -> Handler Html
postUserMainPageR = error "Not yet implemented: postUserMainPageR"

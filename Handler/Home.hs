module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    extra <- getExtra
    defaultLayout $ do
        aDomId <- newIdent
        projectsList <- liftIO $ listProjectIn $ extraProjectsDir extra
        setTitle "Home Page"
        $(widgetFile "homepage")

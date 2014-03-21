module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        aDomId <- newIdent
        let projectsList = []
        setTitle "Home Page"
        $(widgetFile "homepage")

module Handler.About where

import Import

getAboutR :: Handler Html
getAboutR = do
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "LAMIH::About"
        toWidgetBody $(hamletFile "templates/default-head.hamlet")
        $(widgetFile "about")

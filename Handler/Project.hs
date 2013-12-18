module Handler.Project where

import Import

import Data.Git
import Filesystem.Path as FSP
import Filesystem.Path.Rules

toPath :: String -> String -> FSP.FilePath
toPath path dir = (decodeString posix_ghc704 path) </> (decodeString posix_ghc704 dir) </> ".git"

getProjectR :: Text -> Handler Html
getProjectR projectName = do
    isGitProject <- liftIO $ isRepo $ toPath "/home/nicolas/work" $ unpack projectName
    defaultLayout $ do
        aDomId <- newIdent
        setTitle $ toHtml $ "HitWeb::project::" ++ (unpack projectName)
        $(widgetFile "default-head")
        $(widgetFile "project")

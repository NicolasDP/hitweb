module Handler.ProjectShowDiff where

import Import

import Data.Git

import Data.Text as T

import Filesystem.Path as FSP
import Filesystem.Path.Rules as FSP

toPath :: Text -> FSP.FilePath
toPath path = FSP.decodeString posix_ghc704 $ T.unpack path

getProjectShowDiffR :: Text -> Text -> Text -> Handler Html
getProjectShowDiffR projectName oldRef newRef = do
    extra <- getExtra
    let projectsDir = extraProjectsDir extra
    let projectPathT = T.concat [projectsDir,T.pack "/",projectName]
    let projectPathF = toPath projectPathT
    defaultLayout $ do
        isHitProject <- liftIO $ isRepo (projectPathF </> ".git")
        setTitle $ toHtml $ "Hit - " ++ (T.unpack projectName)
        $(widgetFile "default-head")
        if not isHitProject
            then error $ "No such project: " ++ (T.unpack projectName)
            else $(widgetFile "project-show-diff")

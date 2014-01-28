module Handler.ProjectShowDiff where

import Import

import Data.List as L
import Data.Git
import Data.Git.Diff
import Data.Git.Ref
import Data.Algorithm.Patience (Item(..))

import Data.Text as T

import Data.ByteString.Char8 as BC (unpack)
import Data.ByteString.Lazy.Char8 as BL (unpack)

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
        addScriptRemote "http://code.jquery.com/ui/1.10.4/jquery-ui.js"
        $(widgetFile "default-head")
        if not isHitProject
            then error $ "No such project: " ++ (T.unpack projectName)
            else do diffList <- liftIO $ withRepo (projectPathF </> ".git") $ getDiff (fromHexString $ T.unpack oldRef) (fromHexString $ T.unpack newRef)
                    $(widgetFile "project-show-diff")
                    let infiniteList = [1..] :: [Integer]
                    identityDiffFile <- newIdent
                    $(widgetFile "project-show-diff-file")

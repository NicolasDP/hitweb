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

getProjectShowDiffR :: Text -> Text -> Text -> Handler Html
getProjectShowDiffR projectName oldRef newRef = do
    extra <- getExtra
    defaultLayout $ do
        setTitle $ toHtml $ "Hit - " ++ (T.unpack projectName)
        addScriptRemote "http://code.jquery.com/ui/1.10.4/jquery-ui.js"
        hitProjectPath <- liftIO $ getProjectPath (extraProjectsDir extra) projectName
        case hitProjectPath of
            Nothing   -> error $ "No such project: " ++ (T.unpack projectName)
            Just path -> do
                    diffList <- liftIO $ withRepo path $ getDiff (fromHexString $ T.unpack oldRef) (fromHexString $ T.unpack newRef)
                    $(widgetFile "project-show-diff")
                    let infiniteList = [1..] :: [Integer]
                    identityDiffFile <- newIdent
                    $(widgetFile "project-show-diff-file")

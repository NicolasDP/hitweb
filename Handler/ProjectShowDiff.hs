module Handler.ProjectShowDiff where

import Import

import Data.Git
import Data.Git.Diff
import Data.Git.Ref
import Data.Algorithm.Patience (Item(..))

import Data.Text as T

import Data.ByteString.Char8 as BC (unpack)
import Data.ByteString.Lazy.Char8 as BL (unpack)

getProjectShowDiffR :: Text -> Text -> Text -> Text -> Handler Html
getProjectShowDiffR login projName oldRef newRef = do
    extra <- getExtra
    defaultLayout $ do
        setTitle $ toHtml ("Hit - " `mappend` projName)
        hitProjectPath <- liftIO $ getProjectPath (extraProjectsDir extra) login projName
        $(widgetFile "project-show-menu")
        case hitProjectPath of
            Nothing   -> error $ "No such project: " ++ (T.unpack projName)
            Just path -> do
                    diffList <- liftIO $ withRepo path $ getDiff (fromHexString $ T.unpack oldRef) (fromHexString $ T.unpack newRef)
                    $(widgetFile "project-show-diff")
                    identityDiffFile <- newIdent
                    $(widgetFile "project-show-diff-file")

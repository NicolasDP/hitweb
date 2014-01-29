module Handler.ProjectShowTree where

import Import

import Data.Git
import Data.Git.Ref
import Data.Git.Repository
import Data.Text as T (pack,unpack)

import Data.ByteString.Char8 as BC (unpack)

myGetTreeMaybe :: Ref -> Git -> IO HTree
myGetTreeMaybe ref git = do
    tree <- getTreeMaybe git ref
    case tree of
        Just t  -> buildHTree git t
        Nothing -> return []

getProjectShowTreeR :: Text -> Text -> Handler Html
getProjectShowTreeR projectName ref = do
    extra <- getExtra
    defaultLayout $ do
        identityTree <- newIdent
        setTitle $ toHtml $ "Hit - " ++ (T.unpack projectName)
        hitProjectPath <- liftIO $ getProjectPath (extraProjectsDir extra) projectName
        case hitProjectPath of
            Nothing   -> error $ "No such project: " ++ (T.unpack projectName)
            Just path -> do
                let identityNew = T.unpack identityTree
                treeList <- liftIO $ withRepo path $ myGetTreeMaybe $ fromHexString $ T.unpack ref
                $(widgetFile "project-show-tree")

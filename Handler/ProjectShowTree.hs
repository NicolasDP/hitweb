module Handler.ProjectShowTree where

import Import

import Data.Git
import Data.Git.Ref
import Data.Git.Storage
import Data.Git.Revision
import Data.Git.Repository
import Data.Text as T (pack,unpack)

import Data.ByteString.Char8 as BC (unpack)

myGetTreeMaybe :: Ref -> Git -> IO HTree
myGetTreeMaybe ref git = do
    tree <- resolveTreeish git ref
    case tree of
        Just t  -> buildHTree git t
        Nothing -> return []

getProjectShowTreeR :: Text -> Text -> Text -> Handler Html
getProjectShowTreeR login projectName ref = do
    extra <- getExtra
    defaultLayout $ do
        identityTree <- newIdent
        setTitle $ toHtml $ "Hit - " ++ (T.unpack projectName)
        hitProjectPath <- liftIO $ getProjectPath (extraProjectsDir extra) login projectName
        $(widgetFile "project-show-menu")
        case hitProjectPath of
            Nothing   -> error $ "No such project: " ++ (T.unpack projectName)
            Just path -> do
                let stringRef = T.unpack ref
                git <- liftIO $ openRepo path
                newRef <- if (isHexString stringRef)
                              then return $ fromHexString stringRef
                              else let revision = fromString stringRef
                                   in  liftIO $ maybe (error "revision cannot be found") id <$> resolveRevision git revision
                let identityNew = T.unpack identityTree
                treeList <- liftIO $ myGetTreeMaybe newRef git
                $(widgetFile "project-show-tree")
                liftIO $ closeRepo git

module Handler.ProjectShowBlob where

import Import

import Data.Git
import Data.Git.Ref
import Data.Git.Repository
import Data.Git.Storage
import Data.Git.Storage.Object
import Data.Text as T (unpack)

import Data.ByteString.Lazy.Char8 as BL

myGetBlobMaybe :: Ref -> Git -> IO (Maybe ObjectInfo)
myGetBlobMaybe ref git = getObjectRaw git ref True

getProjectShowBlobR :: Text -> Text -> Text -> Handler Html
getProjectShowBlobR login projectName ref = do
    extra <- getExtra
    defaultLayout $ do
        identityTree <- newIdent
        setTitle $ toHtml ("Hit - " `mappend` projectName)
        hitProjectPath <- liftIO $ getProjectPath (extraProjectsDir extra) login projectName
        $(widgetFile "project-show-menu")
        case hitProjectPath of
            Nothing   -> error $ "No such project: " ++ (T.unpack projectName)
            Just path -> do
                let identityNew = T.unpack identityTree
                fileContent <- liftIO $ withRepo path $ myGetBlobMaybe $ fromHexString $ T.unpack ref
                $(widgetFile "project-show-blob")

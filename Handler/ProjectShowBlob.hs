module Handler.ProjectShowBlob where

import Import

import Data.Git
import Data.Git.Ref
import Data.Git.Repository
import Data.Git.Storage
import Data.Git.Storage.Object
import Data.Text as T (pack,unpack,concat)

import Data.ByteString.Lazy.Char8 as BL

import Filesystem.Path as FSP
import Filesystem.Path.Rules as FSP

toPath :: Text -> FSP.FilePath
toPath path = FSP.decodeString posix_ghc704 $ T.unpack path

myGetBlobMaybe :: Ref -> Git -> IO (Maybe ObjectInfo)
myGetBlobMaybe ref git = getObjectRaw git ref True

getProjectShowBlobR :: Text -> Text -> Handler Html
getProjectShowBlobR projectName ref = do
    extra <- getExtra
    let projectsDir = extraProjectsDir extra
    let projectPathT = T.concat [projectsDir,T.pack "/",projectName]
    let projectPathF = toPath projectPathT
    defaultLayout $ do
        identityTree <- newIdent
        isHitProject <- liftIO $ isRepo (projectPathF </> ".git")
        setTitle $ toHtml $ "Hit - " ++ (T.unpack projectName)
        if not isHitProject
            then error $ "No such project: " ++ (T.unpack projectName)
            else do
                let identityNew = T.unpack identityTree
                fileContent <- liftIO $ withRepo (projectPathF </> ".git") $ myGetBlobMaybe $ fromHexString $ T.unpack ref
                $(widgetFile "project-show-blob")

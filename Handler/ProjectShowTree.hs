module Handler.ProjectShowTree where

import Import

import Data.Git
import Data.Git.Ref
import Data.Git.Repository
import Data.Text as T (pack,unpack,concat)

import Data.ByteString.Char8 as BC (unpack)

import Filesystem.Path as FSP
import Filesystem.Path.Rules as FSP

toPath :: Text -> FSP.FilePath
toPath path = FSP.decodeString posix_ghc704 $ T.unpack path

myGetTreeMaybe :: Ref -> Git -> IO HTree
myGetTreeMaybe ref git = do
    tree <- getTreeMaybe git ref
    case tree of
        Just t  -> buildHTree git t
        Nothing -> return []

getProjectShowTreeR :: Text -> Text -> Handler Html
getProjectShowTreeR projectName ref = do
    extra <- getExtra
    maid <- maybeAuthId
    let projectsDir = extraProjectsDir extra
    let projectPathT = T.concat [projectsDir,T.pack "/",projectName]
    let projectPathF = toPath projectPathT
    defaultLayout $ do
        identityTree <- newIdent
        isHitProject <- liftIO $ isRepo (projectPathF </> ".git")
        setTitle $ toHtml $ "Hit - " ++ (T.unpack projectName)
        $(widgetFile "default-head")
        if not isHitProject
            then error $ "No such project: " ++ (T.unpack projectName)
            else do
                let identityNew = T.unpack identityTree
                treeList <- liftIO $ withRepo (projectPathF </> ".git") $ myGetTreeMaybe $ fromHexString $ T.unpack ref
                $(widgetFile "project-show-tree")

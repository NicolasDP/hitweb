module Handler.Project where

import Import
import Data.List as L (head,init, tail)
import Data.List.Split as L (splitOn)
import Data.Text as T (pack,unpack,concat)

import Data.Git
import Data.Git.Types
import Data.Git.Storage
import Data.Git.Revision

import Filesystem.Path as FSP
import Filesystem.Path.Rules as FSP

toPath :: Text -> FSP.FilePath
toPath path = FSP.decodeString posix_ghc704 $ T.unpack path

getLogList' :: Revision -> Data.Git.Storage.Git -> IO [Commit]
getLogList' revision git = do
    ref <- maybe (error "revision cannot be found") id <$> resolveRevision git revision
    loopTillEmpty ref []
    where loopTillEmpty ref list = do
              commit <- getCommit git ref
              case commitParents commit of
                  []    -> return $ list ++ [commit]
                  (p:_) -> loopTillEmpty p (list ++ [commit])

getLogList :: Revision -> FSP.FilePath -> IO [Commit]
getLogList revision path = withRepo path $ getLogList' revision

-- This is the project summary handler.
getProjectR :: Text -> Handler Html
getProjectR projectName  = do
    extra <- getExtra
    let projectsDir = extraProjectsDir extra
    let projectPathT = T.concat [projectsDir,T.pack "/",projectName]
    let projectPathF = toPath projectPathT
    defaultLayout $ do
        aDomId <- newIdent
        isHitProject <- liftIO $ isRepo (projectPathF </> ".git")
        setTitle $ toHtml $ "Hit - " ++ (T.unpack projectName)
        $(widgetFile "default-head")
        if not isHitProject
            then error $ "No such project: " ++ (T.unpack projectName)
            else do
                commits <- liftIO $ getLogList (fromString "master") (projectPathF </> ".git")
                $(widgetFile "project")

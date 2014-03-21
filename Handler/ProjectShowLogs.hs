module Handler.ProjectShowLogs where

import Import

import Data.Git
import Data.Git.Storage
import Data.Git.Ref
import Data.Git.Types
import Data.Git.Revision
import Data.List as L (head, tail, null, last)
import Data.List.Split as L (splitOn)
import Data.Text as T (pack,unpack)
import Data.ByteString.Char8 as BC
import Data.Time.Format

import System.Locale

myGetCommitList :: Ref -> Integer -> Git -> IO [(Ref, Commit)]
myGetCommitList _   0 _   = return []
myGetCommitList ref i git = do
    commit <- getCommit git ref
    case commitParents commit of
        []    -> return [(ref, commit)]
        (p:_) -> ((ref, commit):) <$> (myGetCommitList p (i-1) git)

getProjectShowLogsR :: Text -> Text -> Text -> Integer -> Handler Html
getProjectShowLogsR login projectName ref size = do
    extra <- getExtra
    defaultLayout $ do
        setTitle $ toHtml ("Hit - " `mappend` projectName)
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
                                 in liftIO $ maybe (error "revision cannot be found") id <$> resolveRevision git revision
              commitList <- liftIO $ myGetCommitList newRef size git
              commitId <- newIdent
              $(widgetFile "project-show-log")
              liftIO $ closeRepo git

module Handler.ProjectShowLogs where

import Import

import Data.Git
import Data.Git.Storage
import Data.Git.Ref
import Data.Git.Types
import Data.Git.Revision
import Data.List as L (head, tail)
import Data.List.Split as L (splitOn)
import Data.Text as T (pack,unpack)
import Data.ByteString.Char8 as BC
import Data.Time.Format

import System.Locale

myGetCommitList :: Ref -> Integer -> Git -> IO [Commit]
myGetCommitList _   0 _   = return []
myGetCommitList ref i git = do
    commit <- getCommit git ref
    case commitParents commit of
        []    -> return [commit]
        (p:_) -> (commit:) <$> (myGetCommitList p (i-1) git)

getProjectShowLogsR :: Text -> Text -> Integer -> Handler Html
getProjectShowLogsR projectName ref size = do
    extra <- getExtra
    defaultLayout $ do
        setTitle $ toHtml ("Hit - " `mappend` projectName)
        hitProjectPath <- liftIO $ getProjectPath (extraProjectsDir extra) projectName
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
              showCommits commitList
              liftIO $ closeRepo git
    where --showCommits :: [Commit] -> IO ()
          showCommits [] = return ()
          showCommits (commit:xs) = do
              let message = L.splitOn "\n" $ BC.unpack $ commitMessage commit
              commitHeaderId <- newIdent
              commitId <- newIdent
              $(widgetFile "project-show-commit")
              showCommits xs

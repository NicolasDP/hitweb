module Handler.ProjectShowCommit where

import Import

import Data.Git
import Data.Git.Storage
import Data.Git.Ref
import Data.Git.Repository
import Data.Git.Types
import Data.List as L (head, tail)
import Data.List.Split as L (splitOn)
import Data.Text as T (pack,unpack)
import Data.ByteString.Char8 as BC
import Data.Time.Format

import System.Locale

myGetCommit :: Ref -> Git -> IO (Maybe Commit)
myGetCommit ref git = getCommitMaybe git ref

getProjectShowCommitR :: Text -> Text -> Handler Html
getProjectShowCommitR projectName ref = do
    extra <- getExtra
    defaultLayout $ do
        setTitle $ toHtml $ "Hit - " ++ (T.unpack projectName)
        hitProjectPath <- liftIO $ getProjectPath (extraProjectsDir extra) projectName
        case hitProjectPath of
            Nothing   -> error $ "No such project: " ++ (T.unpack projectName)
            Just path -> do
                commitMaybe <- liftIO $ withRepo path $ myGetCommit $ fromHexString $ T.unpack ref
                case commitMaybe of
                    Just commit -> do let message = L.splitOn "\n" $ BC.unpack $ commitMessage commit
                                      $(widgetFile "project-show-commit")
                    Nothing     -> error $ "Ref \"" ++ (T.unpack ref) ++ "\" unknown for project: " ++ (T.unpack projectName)

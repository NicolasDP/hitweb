module Handler.ProjectShowCommit where

import Import

import Data.Git
import Data.Git.Diff
import Data.Git.Storage
import Data.Git.Ref
import Data.Git.Repository
import Data.Git.Types
import Data.List as L (head, tail)
import Data.List.Split as L (splitOn)
import Data.Text as T (pack,unpack)
import Data.ByteString.Char8 as BC
import Data.ByteString.Lazy.Char8 as BL (unpack)
import Data.Time.Format

import Data.Algorithm.Patience (Item(..))
import System.Locale

myGetCommit :: Ref -> Git -> IO (Maybe Commit)
myGetCommit ref git = getCommitMaybe git ref

getProjectShowCommitR :: Text -> Text -> Text -> Handler Html
getProjectShowCommitR login projectName ref = do
    let currentRef = ref
    extra <- getExtra
    defaultLayout $ do
        setTitle $ toHtml ("Hit - " `mappend` projectName)
        hitProjectPath <- liftIO $ getProjectPath (extraProjectsDir extra) login projectName
        $(widgetFile "project-show-menu")
        case hitProjectPath of
            Nothing   -> error $ "No such project: " ++ (T.unpack projectName)
            Just path -> do
                commitMaybe <- liftIO $ withRepo path $ myGetCommit $ fromHexString $ T.unpack ref
                case commitMaybe of
                    Just commit -> do let message = L.splitOn "\n" $ BC.unpack $ commitMessage commit
                                      commitHeaderId <- newIdent
                                      commitId <- newIdent
                                      $(widgetFile "project-show-commit")
                                      diffList <- liftIO $ withRepo path $ getDiff (L.head $ commitParents commit) (fromHexString $ T.unpack ref)
                                      identityDiffFile <- newIdent
                                      $(widgetFile "project-show-diff-file")
                    Nothing     -> error $ "Ref \"" ++ (T.unpack ref) ++ "\" unknown for project: " ++ (T.unpack projectName)

module Handler.ProjectShowCommit where

import Import

import Data.Git
import Data.Git.Storage
import Data.Git.Ref
import Data.Git.Repository
import Data.Git.Types
import Data.List as L (head, tail)
import Data.List.Split as L (splitOn)
import Data.Text as T (pack,unpack,concat)
import Data.ByteString.Char8 as BC
import Data.Time.Format

import System.Locale

import Filesystem.Path as FSP
import Filesystem.Path.Rules as FSP

toPath :: Text -> FSP.FilePath
toPath path = FSP.decodeString posix_ghc704 $ T.unpack path

myGetCommit :: Ref -> Git -> IO (Maybe Commit)
myGetCommit ref git = getCommitMaybe git ref

getProjectShowCommitR :: Text -> Text -> Handler Html
getProjectShowCommitR projectName ref = do
    extra <- getExtra
    let projectsDir = extraProjectsDir extra
    let projectPathT = T.concat [projectsDir,T.pack "/",projectName]
    let projectPathF = toPath projectPathT
    defaultLayout $ do
        isHitProject <- liftIO $ isRepo (projectPathF </> ".git")
        setTitle $ toHtml $ "Hit - " ++ (T.unpack projectName)
        $(widgetFile "default-head")
        if not isHitProject
            then error $ "No such project: " ++ (T.unpack projectName)
            else do
                commitMaybe <- liftIO $ withRepo (projectPathF </> ".git") $ myGetCommit $ fromHexString $ T.unpack ref
                case commitMaybe of
                    Just commit -> do let message = L.splitOn "\n" $ BC.unpack $ commitMessage commit
                                      $(widgetFile "project-show-commit")
                    Nothing     -> error $ "Ref \"" ++ (T.unpack ref) ++ "\" unknown for project: " ++ (T.unpack projectName)

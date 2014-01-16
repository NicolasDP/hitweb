module Handler.Projects where

import Import

import Data.Git
import Data.Git.Storage
import Data.Git.Revision
import Data.Maybe
import Data.List as L (intersperse, concat, init, tail, head)
import Data.List.Split as L (splitOn)
import Data.Text as T (pack, unpack, splitOn)

import System.Time

import System.Directory

import Filesystem.Path as FSP
import Filesystem.Path.Rules

data HPNode = HPDir String | HPRepo String
  deriving (Show, Eq, Ord)

toPath :: String -> String -> FSP.FilePath
toPath path dir = (decodeString posix_ghc704 path) </> (decodeString posix_ghc704 dir)

toGitPath :: String -> String -> FSP.FilePath
toGitPath path dir = (toPath path dir) </> ".git"

insertProject :: String -> String -> IO (Maybe HPNode)
insertProject path repo = do
    isHitProject <- isRepo $ toGitPath path repo
    isDir <- doesDirectoryExist $ encodeString posix $ toPath path repo
    if (isHitProject)
        then return $ Just $ HPRepo repo
        else if isDir
            then return $ Just $ HPDir repo
            else return Nothing

getProjectsName :: String -> String -> IO [Maybe HPNode]
getProjectsName hitwebPath "" = do
    contents <- getDirectoryContents $ hitwebPath
    mapM (insertProject hitwebPath) $ filter (not . flip elem [".", ".."]) contents
getProjectsName hitwebPath path = do
    contents <- getDirectoryContents $ hitwebPath ++ "/" ++ path
    mapM (insertProject hitwebPath) $ map ((path ++ "/") ++) $ filter (not . flip elem [".", ".."]) contents

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

fromGitToClockTime :: GitTime -> ClockTime
fromGitToClockTime (GitTime a b) = TOD a $ toInteger b

-- Projects directory handler
-- Generate the subtree project for display (see templates/projects.h
-- file).
getProjectsR :: [Text] -> Handler Html
getProjectsR list = do
    extra <- getExtra
    let path = (L.concat $ L.intersperse "/" $ map T.unpack list)
    let fullPath = extraHitwebDir extra
    let upPath = L.init $ T.splitOn "/" $ T.pack path
    defaultLayout $ do
        aDomId <- newIdent
        projectsList <- liftIO $ getProjectsName fullPath path
        isHitProject <- liftIO $ isRepo $ toGitPath fullPath path
        setTitle $ toHtml $ "HitWeb::project::" ++ path
        $(widgetFile "default-head")
        if not isHitProject
            then $(widgetFile "projects_showtree")
            else do
                commits <- liftIO $ getLogList (fromString "master") (toGitPath fullPath path)
                $(widgetFile "project")

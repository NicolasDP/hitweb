module Handler.Projects where

import Import

import Data.Git
import Data.Maybe
import Data.List as L (intersperse, concat)
import Data.Text as T (pack, unpack)

import System.Directory

import Filesystem.Path as FSP
import Filesystem.Path.Rules

data HPNode = HPDir String | HPRepo String

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

getProjectsR :: [Text] -> Handler Html
getProjectsR list = do
    extra <- getExtra
    let path = (L.concat $ L.intersperse "/" $ map T.unpack list)
    let fullPath = extraHitwebDir extra
    defaultLayout $ do
        aDomId <- newIdent
        projectsList <- liftIO $ getProjectsName fullPath path
        setTitle $ toHtml $ "HitWeb::projects::" ++ path
        $(widgetFile "default-head")
        $(widgetFile "projects")

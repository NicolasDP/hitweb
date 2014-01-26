module Handler.Project where

import Import
import Data.List as L (head, init, tail)
import Data.List.Split as L (splitOn)
import Data.Text as T (pack,unpack,concat)

import Data.Git
import Data.Git.Types
import Data.Git.Storage
import Data.Git.Revision

import Data.Time.Format
import System.Locale

import Data.ByteString.Char8 as BC (unpack)

import Filesystem.Path as FSP
import Filesystem.Path.Rules as FSP

toPath :: Text -> FSP.FilePath
toPath path = FSP.decodeString posix_ghc704 $ T.unpack path

getLogList' :: Revision -> Data.Git.Storage.Git -> IO [(Commit,Ref)]
getLogList' revision git = do
    ref <- maybe (error "revision cannot be found") id <$> resolveRevision git revision
    loopTill 10 ref []
    where loopTill :: Integer -> Ref -> [(Commit,Ref)] -> IO [(Commit,Ref)]
          loopTill 0 _   list = return list
          loopTill i ref list = do
              commit <- getCommit git ref
              case commitParents commit of
                  []    -> return $ list ++ [(commit,ref)]
                  (p:_) -> loopTill (i-1) p (list ++ [(commit,ref)])

getLogList :: Revision -> FSP.FilePath -> IO [(Commit,Ref)]
getLogList revision path = withRepo path $ getLogList' revision

-- This is the project summary handler.
getProjectR :: Text -> Handler Html
getProjectR projectName  = do
    extra <- getExtra
    let projectsDir = extraProjectsDir extra
    let projectPathT = T.concat [projectsDir,T.pack "/",projectName]
    let projectPathF = toPath projectPathT
    defaultLayout $ do
        identityDescription <- newIdent
        identityLogList <- newIdent
        isHitProject <- liftIO $ isRepo (projectPathF </> ".git")
        setTitle $ toHtml $ "Hit - " ++ (T.unpack projectName)
        $(widgetFile "default-head")
        if not isHitProject
            then error $ "No such project: " ++ (T.unpack projectName)
            else do
                commits <- liftIO $ getLogList (fromString "master") (projectPathF </> ".git")
                description <- liftIO $ withRepo (projectPathF </> ".git") $ getDescription
                $(widgetFile "project")

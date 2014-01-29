module Handler.Project where

import Import
import Data.List as L (head, init, tail)
import Data.List.Split as L (splitOn)
import Data.Text as T (unpack, pack)

import Data.Git
import Data.Git.Types
import Data.Git.Storage
import Data.Git.Revision

import Data.Time.Format
import System.Locale

import Data.ByteString.Char8 as BC (unpack)

getLogList :: Revision -> Data.Git.Storage.Git -> IO [(Commit,Ref)]
getLogList revision git = do
    ref <- maybe (error "revision cannot be found") id <$> resolveRevision git revision
    loopTill 10 ref []
    where loopTill :: Integer -> Ref -> [(Commit,Ref)] -> IO [(Commit,Ref)]
          loopTill 0 _   list = return list
          loopTill i ref list = do
              commit <- getCommit git ref
              case commitParents commit of
                  []    -> return $ list ++ [(commit,ref)]
                  (p:_) -> loopTill (i-1) p (list ++ [(commit,ref)])

-- This is the project summary handler.
getProjectR :: Text -> Handler Html
getProjectR projectName  = do
    extra <- getExtra
    defaultLayout $ do
        identityDescription <- newIdent
        identityLogList <- newIdent
        setTitle $ toHtml $ "Hit - " ++ (T.unpack projectName)
        hitProjectPath <- liftIO $ getProjectPath (extraProjectsDir extra) projectName
        case hitProjectPath of
            Nothing   -> error $ "No such project: " ++ (T.unpack projectName)
            Just path -> do
                commits     <- liftIO $ withRepo path $ getLogList (fromString "master")
                description <- liftIO $ withRepo path $ getDescription
                $(widgetFile "project")

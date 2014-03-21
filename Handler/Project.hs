{-# LANGUAGE OverloadedStrings #-}
module Handler.Project where

import Import
import Data.List as L (head, init, tail)
import Data.List.Split as L (splitOn)
import Data.Text as T (unpack, pack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL (decodeUtf8)

import Cheapskate

import Data.Git
import Data.Git.Types
import Data.Git.Storage
import Data.Git.Storage.Object
import Data.Git.Revision

import Data.Time.Format
import System.Locale
import qualified Data.ByteString.Char8 as BC

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

getReadmeContent :: Revision -> Data.Git.Storage.Git -> IO (Maybe Html)
getReadmeContent rev git = do
    ref <- maybe (error "revision cannot be found") id <$> resolveRevision git rev
    readmeRef <- resolvePath git ref ["README.md"]
    case readmeRef of
        Nothing  -> return Nothing
        Just bRef -> do
            contentMaybe <- getObjectRaw git bRef True
            case contentMaybe of
                Nothing      -> return Nothing
                Just content ->
                    let mdContent = markdown def $ TL.toStrict $ TL.decodeUtf8 $ oiData content
                    in  return $ Just $ toHtml mdContent

-- This is the project summary handler.
getProjectR :: Text -> Text -> Handler Html
getProjectR login projectName  = do
    extra <- getExtra
    defaultLayout $ do
        identityDescription <- newIdent
        identityLogList <- newIdent
        identityReadme <- newIdent
        setTitle $ toHtml ("Hit - " `mappend` projectName)
        hitProjectPath <- liftIO $ getProjectPath (extraProjectsDir extra) login projectName
        $(widgetFile "project-show-menu")
        case hitProjectPath of
            Nothing   -> error $ "No such project: " ++ (T.unpack projectName)
            Just path -> do
                commits     <- liftIO $ withRepo path $ getLogList (fromString "master")
                description <- liftIO $ withRepo path $ getDescription
                readmeMD    <- liftIO $ withRepo path $ getReadmeContent (fromString "master")
                $(widgetFile "project")

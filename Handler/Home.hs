{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Handler.Home where

import Import

import Data.Maybe
import Data.Git
import Data.Text
import System.Directory

import Filesystem.Path as FSP
import Filesystem.Path.Rules

toPath :: String -> String -> FSP.FilePath
toPath path dir = (decodeString posix_ghc704 path) </> (decodeString posix_ghc704 dir) </> ".git"

insertProject :: String -> String -> IO (Maybe String)
insertProject path repo = do
  isHitProject <- isRepo $ toPath path repo
  if (isHitProject) then
    return $ Just repo
  else do
    return Nothing

getProjectsName :: String -> IO [Maybe String]
getProjectsName path = do
  contents <- getDirectoryContents path
  mapM (insertProject path) contents

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    defaultLayout $ do
        aDomId <- newIdent
        projectsList <- liftIO $ getProjectsName "/home/nicolas/work/yesod"
        setTitle "HitWeb::home"
        $(widgetFile "default-head")
        $(widgetFile "homepage")

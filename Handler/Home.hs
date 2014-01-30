{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import Import

import Data.Git.Storage (isRepo)

import Filesystem.Path as FSP
import Filesystem.Path.CurrentOS as FSP
import Filesystem

getProjectsName :: Text -> IO [Text]
getProjectsName (FSP.fromText -> hitwebPath) =
    foldr f (return []) =<< listDirectory hitwebPath
  where f :: FSP.FilePath -> IO [Text] -> IO [Text]
        f t accu = do
            let gitPath = hitwebPath </> t </> ".git"
            isHitProject <- isRepo gitPath
            if isHitProject then (either id id (FSP.toText t):) <$> accu
                            else accu

getHomeR :: Handler Html
getHomeR = do
    extra <- getExtra
    defaultLayout $ do
        aDomId <- newIdent
        projectsList <- liftIO $ getProjectsName $ extraProjectsDir extra
        setTitle "Home Page"
        $(widgetFile "homepage")

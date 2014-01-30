{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Hitweb.Filepath
    ( getProjectPath
    , listProjectIn
    ) where

import Prelude
import Control.Applicative

import Data.Git.Storage

import Data.Text as T (Text)

import Filesystem
import Filesystem.Path as FSP
import Filesystem.Path.CurrentOS as FSP

-- | for the hitweb projects directory (dir), return the path to the git
-- directory:
--   Just dir/projectName (is a git directory - project.git)
--   Just git/projectName/.git (is a git directory)
--   Nothing
--
getProjectPath :: Text -> Text -> IO (Maybe FSP.FilePath)
getProjectPath (FSP.fromText -> dir)
               (FSP.fromText -> projectName) =
    getProjectPath' dir projectName

getProjectPath' :: FSP.FilePath -> FSP.FilePath -> IO (Maybe FSP.FilePath)
getProjectPath' dir projectName = do
    let path1 = dir </> projectName
    let path2 = path1 </> ".git"
    isGitProj1 <- isRepo path1
    isGitProj2 <- isRepo path2
    return $ case (isGitProj1, isGitProj2) of
        (True , _   ) -> Just path1
        (False, True) -> Just path2
        _             -> Nothing

-- | list all of the current projects in this root directory
listProjectIn :: Text -> IO [Text]
listProjectIn (FSP.fromText -> rootPath) =
    foldr f (return []) =<< listDirectory rootPath
  where f :: FSP.FilePath -> IO [Text] -> IO [Text]
        f t accu = do
            pathMaybe <- getProjectPath' rootPath t
            case pathMaybe of
                Just _  -> (either id id (FSP.toText $ FSP.filename t):) <$> accu
                Nothing -> accu

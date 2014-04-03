{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Hitweb.Filepath
    ( getProjectPath
    , listProjectIn
    , createUserIn
    , deleteUserIn
    ) where

import Prelude
import Control.Applicative
import Data.Git.Storage
import Data.Text                 as T (Text)
import Filesystem                as FSP
import Filesystem.Path           as FSP
import Filesystem.Path.CurrentOS as FSP

-- | for the hitweb projects directory (dir), return the path to the git
-- directory:
--   Just dir/projectName (is a git directory - project.git)
--   Just git/projectName/.git (is a git directory)
--   Nothing
--
getProjectPath :: Text -> Text -> Text -> IO (Maybe FSP.FilePath)
getProjectPath (FSP.fromText -> dir)
               (FSP.fromText -> login)
               (FSP.fromText -> projectName) =
    getProjectPath' dir login projectName

getProjectPath' :: FSP.FilePath -> FSP.FilePath -> FSP.FilePath -> IO (Maybe FSP.FilePath)
getProjectPath' dir login projectName = do
    let path1 = dir </> login </> projectName
    let path2 = path1 </> ".git"
    isGitProj1 <- isRepo path1
    isGitProj2 <- isRepo path2
    return $ case (isGitProj1, isGitProj2) of
        (True , _   ) -> Just path1
        (False, True) -> Just path2
        _             -> Nothing

-- | list all of the current projects in the given directory
listProjectIn :: Text -> Text -> IO [Text]
listProjectIn (FSP.fromText -> rootPath)
              (FSP.fromText -> login) =
    foldr f (return []) =<< (listDirectory $ rootPath </> login)
  where f :: FSP.FilePath -> IO [Text] -> IO [Text]
        f t accu = do
            pathMaybe <- getProjectPath' rootPath login t
            case pathMaybe of
                Just _  -> (either id id (FSP.toText $ FSP.filename t):) <$> accu
                Nothing -> accu

createUserIn :: Text -> Text -> IO ()
createUserIn (FSP.fromText -> dir)
             (FSP.fromText -> user) =
    createDirectory False $ dir </> user

deleteUserIn :: Text -> Text -> IO ()
deleteUserIn (FSP.fromText -> dir)
             (FSP.fromText -> user) =
     removeTree $ dir </> user

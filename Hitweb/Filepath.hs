module Hitweb.Filepath
    ( getProjectPath
    ) where

import Prelude

import Data.Git.Storage

import Data.Text as T (Text, unpack)

import Filesystem.Path as FSP
import Filesystem.Path.Rules as FSP

-- | for the hitweb projects directory (dir), return the path to the git
-- directory:
--   Just dir/projectName (is a git directory - project.git)
--   Just git/projectName/.git (is a git directory)
--   Nothing
getProjectPath :: Text -> Text -> IO (Maybe FSP.FilePath)
getProjectPath dir projectName = do
    let path1 = (toPath dir) </> (toPath projectName)
    let path2 = path1 </> ".git"
    isGitProj1 <- isRepo path1
    isGitProj2 <- isRepo path2
    return $ case (isGitProj1, isGitProj2) of
        (True , _   ) -> Just path1
        (False, True) -> Just path2
        _             -> Nothing
    where toPath :: Text -> FSP.FilePath
          toPath path = FSP.decodeString posix_ghc704 $ T.unpack path

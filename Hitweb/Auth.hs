module Hitweb.Auth
    ( doesProjectRequiredAuth
    , projectAuthAnybody
    , doesUserIsAuthorized
    ) where

import Prelude
import Hitweb.Filepath (getProjectPath)
import Yesod           (AuthResult(..))
import Yesod.Auth      ()
import Data.Text       as T (Text, concat, lines)
import Filesystem      as FSP
import Filesystem.Path as FSP

import Model

projectAuthorizedFileName :: FSP.FilePath
projectAuthorizedFileName = "hitweb.authorized"

-- | it is possible to allow access to only 'logged' users.
-- then, write this line into the file 'projectAuthorizedFileName' to
-- authorize any authenticated users.
projectAuthAnybody :: T.Text
projectAuthAnybody = "anybody"

-- | check if a project required any authorization
-- i.e.: check if the file 'projectAuthorizedFilename' exist for the
-- 'projName' in the directory 'dirPath'
doesProjectRequiredAuth :: T.Text -> T.Text -> T.Text -> IO Bool
doesProjectRequiredAuth dirPath login projName = do
    pathMaybe <- getProjectPath dirPath login projName
    case pathMaybe of
        Nothing   -> return False
        Just path -> do
            isFile $ path </> projectAuthorizedFileName

-- | check if a user (userIdent) is allowed to access a project (projName).
doesUserIsAuthorized :: T.Text -> T.Text -> T.Text -> Maybe Identity -> IO AuthResult
-- | If a user is not logged (Nothing) then he should.
doesUserIsAuthorized _       _           _     Nothing          = return AuthenticationRequired
-- | If a user is logged (Just userIdent) then we check
--    if he is in the list
-- OR if the project (projName) allows any logged user.
doesUserIsAuthorized dirPath login projName (Just user) = do
    pathMaybe <- getProjectPath dirPath login projName
    case pathMaybe of
        Nothing   -> return $ Unauthorized $ T.concat ["No project named: ", projName]
        Just path -> do
            putStrLn $ show $ path </> projectAuthorizedFileName
            contents <- FSP.readTextFile $ path </> projectAuthorizedFileName
            let contentLines = T.lines contents
            return $ userIsIn contentLines
    where userIsIn :: [T.Text] -> AuthResult
          userIsIn []        =
              Unauthorized $ T.concat [identityIdent user,": user not authorized to access project '",projName,"'"]
          userIsIn (line:xs) =
              if elem line [identityIdent user,projectAuthAnybody] then Authorized else userIsIn xs

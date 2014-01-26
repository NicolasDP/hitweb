module Handler.Home where

import Import

import Data.Git.Storage (isRepo)

import Data.Text as T (pack, unpack)

import System.Directory (getDirectoryContents)
import Filesystem.Path as FSP
import Filesystem.Path.Rules as FSP

toPath :: Text -> Text -> FSP.FilePath
toPath dir path = (decodeString posix_ghc704 $ T.unpack dir)
                  </> (decodeString posix_ghc704 $ T.unpack path)

getProjectsName :: Text -> IO [Text]
getProjectsName hitwebPath = do
    contents <- getDirectoryContents $ T.unpack hitwebPath
    foldr f (return []) $ map T.pack $ Import.filter (not.flip elem [".",".."]) contents
    where f :: Text -> IO [Text] -> IO [Text]
          f t accu = do
                let path = toPath hitwebPath t
                let gitPath = path </> ".git"
                isHitProject <- isRepo gitPath
                if isHitProject then do tmp <- accu
                                        return $ t:tmp
                                else accu

getHomeR :: Handler Html
getHomeR = do
    extra <- getExtra
    defaultLayout $ do
        aDomId <- newIdent
        projectsList <- liftIO $ getProjectsName $ extraProjectsDir extra
        setTitle "Home Page"
        $(widgetFile "default-head")
        $(widgetFile "homepage")

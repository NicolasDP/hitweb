{-# LANGUAGE OverloadedStrings #-}
module Handler.SettingsUser where

import Import

------------------------------------------------------------------------------
-- Main setting page ---------------------------------------------------------
------------------------------------------------------------------------------

getSettingsUserR :: Handler Html
getSettingsUserR = do
    userIdentity <- maybeAuth
    userSettingsWidgetId <- newIdent
    case userIdentity of
       Nothing           -> do
          setMessage "use login system to create a new user first"
          redirect HomeR
       Just (Entity i _) -> do
          defaultLayout $ do
              setTitle "Hit - User: Create"
              $(widgetFile "settings")



postSettingsUserR :: Handler Html
postSettingsUserR = error "Not yet implemented: postSettingsUserR"

------------------------------------------------------------------------------
-- Deleting user -------------------------------------------------------------
------------------------------------------------------------------------------

getSettingsUserDeleteR :: Handler Html
getSettingsUserDeleteR = do
    userIdentity <- maybeAuth
    userDeleteWidgetId <- newIdent
    case userIdentity of
       Nothing           -> do
          setMessage "use login system to create a new user first"
          redirect HomeR
       Just (Entity i _) -> do
          defaultLayout $ do
              setTitle "Hit - User:Settings:Delete"
              $(widgetFile "settings-user-delete")


postSettingsUserDeleteR :: Handler Html
postSettingsUserDeleteR = do
    extra <- getExtra
    userIdentity <- maybeAuth
    case userIdentity of
       Nothing           -> do
          setMessage "use login system to create a new user first"
          redirect HomeR
       Just (Entity i _) -> do
           user <- runDB $ selectFirst [UserIdent ==. i] []
           runDB $ do
               deleteWhere [ProjectIdent ==. i]
               deleteWhere [UserIdent ==. i]
               delete i
           case user of
               Just (Entity _ u) -> lift $ deleteUserIn (extraProjectsDir extra) (userLogin u)
               _                 -> return ()
           setMessage "Your account has been deleted"
           redirect HomeR
	   --
------------------------------------------------------------------------------
-- Project Main --------------------------------------------------------------
------------------------------------------------------------------------------

getSettingsUserProjectR :: Handler Html
getSettingsUserProjectR = do
    userIdentity <- maybeAuth
    settingsUserProjectWidgetId <- newIdent
    case userIdentity of
       Nothing           -> do
          setMessage "use login system to create a new user first"
          redirect HomeR
       Just (Entity i _) -> do
          userProjList <- runDB $ selectList [ProjectIdent ==. i] []
          [Entity _ u] <- runDB $ selectList [UserIdent ==. i] []
          defaultLayout $ do
              setTitle "Hit - User:Settings:Projects"
              $(widgetFile "settings-user-projects")

postSettingsUserProjectR :: Handler Html
postSettingsUserProjectR = error "Not yet implemented"

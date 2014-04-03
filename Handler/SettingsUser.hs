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
          defaultLayout $ do
              setTitle "Hit - User:Settings:Projects"
              $(widgetFile "settings-user-projects")

postSettingsUserProjectR :: Handler Html
postSettingsUserProjectR = error "Not yet implemented"

------------------------------------------------------------------------------
-- Project Create ------------------------------------------------------------
------------------------------------------------------------------------------

projectCreationForm :: Key Identity -> Form Project
projectCreationForm identityId = renderDivs $ Project
    <$> areq textField "Project name" Nothing
    <*> pure identityId

getSettingsUserProjectCreateR :: Handler Html
getSettingsUserProjectCreateR = do
    userIdentity <- maybeAuth
    settingsUserProjectCreateWidgetId <- newIdent
    case userIdentity of
       Nothing           -> do
          setMessage "use login system to create a new user first"
          redirect HomeR
       Just (Entity i _) -> do
          (projectCreationFormWidget, enctype) <- generateFormPost $ projectCreationForm i
          defaultLayout $ do
              setTitle "Hit - User:Settings:Project:Create"
              $(widgetFile "settings-user-project-create")

postSettingsUserProjectCreateR :: Handler Html
postSettingsUserProjectCreateR = do
    extra <- getExtra
    userIdentity <- maybeAuth
    case userIdentity of
       Nothing           -> do
          setMessage "use login system to create a new user first"
          redirect $ AuthR LoginR
       Just (Entity i _) -> do
           ((res,_),_) <- runFormPost $ projectCreationForm i
           case res of
              FormSuccess proj -> do
                  m <- runDB $ selectFirst [ProjectName ==. (projectName proj), ProjectIdent ==. i] []
                  user <- runDB $ selectFirst [UserIdent ==. i] []
                  case (m, user) of
                      (Nothing, Just (Entity _ u)) -> do lift $ createProjectIn (extraProjectsDir extra) (userLogin u) (projectName proj)
                                                         _ <- runDB $ insert proj
                                                         setMessage $ toHtml $ "project created: " ++ (show $ projectName proj)
                                                         redirect SettingsUserProjectR
                      (Just _, _)                  -> do setMessage "Project already exist"
                                                         redirect SettingsUserProjectCreateR
                      _                            -> do setMessage "Unknown error, contact the administrator"
                                                         redirect SettingsUserProjectCreateR
              _ -> do
                  setMessage "error: ... incorrect... TODO"
                  redirect SettingsUserProjectCreateR

------------------------------------------------------------------------------
-- Project Delete ------------------------------------------------------------
------------------------------------------------------------------------------

getSettingsUserProjectDeleteR :: [Text] -> Handler Html
getSettingsUserProjectDeleteR = error "Not yet implemented"

postSettingsUserProjectDeleteR :: [Text] -> Handler Html
postSettingsUserProjectDeleteR = error "Not yet implemented"


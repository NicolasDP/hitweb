module Handler.SettingsProject where

import Import

getSettingsProjectR :: Text -> Text -> Handler Html
getSettingsProjectR login projName = do
    userIdentity <- maybeAuth
    projectSettingsWidgetId <- newIdent
    case userIdentity of
       Nothing           -> do
          setMessage "use login system to create a new user first"
          redirect HomeR
       Just (Entity i _) -> do
          defaultLayout $ do
              setTitle "Hit - Settings:Project"
              $(widgetFile "settings-project")

postSettingsProjectR :: Text -> Text -> Handler Html
postSettingsProjectR = error "Not yet implemented: postSettingsProjectR"

------------------------------------------------------------------------------
-- Project Create ------------------------------------------------------------
------------------------------------------------------------------------------

projectCreationForm :: Key Identity -> Text -> Form Project
projectCreationForm identityId projName = renderDivs $ Project
    <$> areq textField "Project name" (Just projName)
    <*> pure identityId

getSettingsProjectCreateR :: Text -> Text -> Handler Html
getSettingsProjectCreateR _ projName = do
    userIdentity <- maybeAuth
    settingsUserProjectCreateWidgetId <- newIdent
    case userIdentity of
       Nothing           -> do
          setMessage "use login system to create a new user first"
          redirect HomeR
       Just (Entity i _) -> do
          (projectCreationFormWidget, enctype) <- generateFormPost $ projectCreationForm i projName
          defaultLayout $ do
              setTitle "Hit - User:Settings:Project:Create"
              $(widgetFile "settings-user-project-create")

postSettingsProjectCreateR :: Text -> Text -> Handler Html
postSettingsProjectCreateR login projName = do
    extra <- getExtra
    userIdentity <- maybeAuth
    case userIdentity of
       Nothing           -> do
          setMessage "use login system to create a new user first"
          redirect $ AuthR LoginR
       Just (Entity i _) -> do
           ((res,_),_) <- runFormPost $ projectCreationForm i projName
           case res of
              FormSuccess proj -> do
                  m <- runDB $ selectFirst [ProjectName ==. (projectName proj), ProjectIdent ==. i] []
                  user <- runDB $ selectFirst [UserIdent ==. i] []
                  case (m, user) of
                      (Nothing, Just (Entity _ u)) -> do lift $ createProjectIn (extraProjectsDir extra) (userLogin u) (projectName proj)
                                                         _ <- runDB $ insert proj
                                                         setMessage $ toHtml $ "project created: " ++ (show $ projectName proj)
                                                         redirect $ SettingsProjectR login $ projectName proj
                      (Just _, _)                  -> do setMessage "Project already exist"
                                                         redirect $ SettingsProjectCreateR login $ projectName proj
                      _                            -> do setMessage "Unknown error, contact the administrator"
                                                         redirect $ SettingsProjectCreateR login $ projectName proj
              _ -> do
                  setMessage "error: ... incorrect... TODO"
                  redirect HomeR

------------------------------------------------------------------------------
-- Project Delete ------------------------------------------------------------
------------------------------------------------------------------------------

getSettingsProjectDeleteR :: Text -> Text -> Handler Html
getSettingsProjectDeleteR login projName = do
    userIdentity <- maybeAuth
    settingsUserProjectDeleteWidgetId <- newIdent
    case userIdentity of
       Nothing           -> do
          setMessage "use login system to create a new user first"
          redirect HomeR
       Just (Entity i _) -> do
          defaultLayout $ do
              setTitle "Hit - User:Settings:Delete"
              $(widgetFile "settings-user-project-delete")


postSettingsProjectDeleteR :: Text -> Text -> Handler Html
postSettingsProjectDeleteR login projName = do
    extra <- getExtra
    userIdentity <- maybeAuth
    case userIdentity of
       Nothing           -> do
          setMessage "use login system to create a new user first"
          redirect HomeR
       Just (Entity i _) -> do
           -- TODO: check right before deleting a project
           runDB $ deleteWhere [ProjectIdent ==. i, ProjectName ==. projName]
           lift $ deleteProjectIn (extraProjectsDir extra) login projName
           setMessage "Your project has been deleted"
           redirect SettingsUserProjectR
